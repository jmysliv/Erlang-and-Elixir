Elixir GenStage
===

GenStage to narzędzię, które pozwala na wymianę zdarzeń, danych pomiędzy procesami, z odwróconym przepływem żądań. Chodzi w skrócie o to, że proces żądą taką ilość danych jaką może swobodnie przetwożyć i taką właśnie otrzymuje.

## Cel

Jaki był właściwie powód powstania GenStage? Jose Valim chciał wprowadzić narzędzię które pozwoli na efektywne przetwarzanie danych. Jako przykład podam tu problem liczenia słów zaprezentowany na jego blogu.

```elixir
File.stream!("path")
|> Stream.flat_map(fn line ->
    String.split(line, " ")
   end)
|> Stream.async()
|> Enum.reduce(%{}, fn word, acc ->
    Map.update(acc, word, 1, & &1 + 1)
   end)
|> Enum.to_list()
```

Rozwiązanie przedstawione powyżej ma pewne wady, które występowały ogólnie podczas przetwarzania danych w ten sposób:
* Ograniczenie przesyłania danych między procesami
* Brak możliwości sprawnego uruchomienia kilku procesów przeprowadzających te same obliczenia, z innymi danymi równolegle
* Brak nadzoru nad nowymi procesami
* Nie można zapobiec wysłaniu do procesu zbyt dużej ilości danych.

Szczególnie to ostatnie może być niebezpieczne i może się przytrafić jeśli produkowanych jest więcej danych niż proces może przetwożyć. Dobrym przykładem w tym wypadku jest fabryka, w której pracownicy nie nadążają pakować wytwarzanych produktów.

<p align=center>
  <img src=images/overflow.gif/>
</p>



## Historia

Powyższe problemy udało się rozwiązać w 2016 roku, kiedy to 14 lipca oficjalnie wypuszczono GenStage. Od tamtego czasu GenStage jest nieustannie rozwijany, głownie przez Jose Valima. Po kilku latach rozwoju, w lutym tego roku doczekaliśmy się wersji v1.0.0 Elixir GenStage. 

<p align=center>
  <img src=images/contributions.png/>
</p>

GenStage był wprowadzany jako narzędzię do przetwarzania danych, ale nie tylko. Jednym z celów podczas wprowadzanie GenStage było również zastąpienie GenEvent. Rozwiązywał on kilka problemów związanych z GenEvent między innymi to, że zarówno menadżer zdarzeń jak i ich obsługa działały w jednym procesie. 

## Opis działania

GenStages wyróżnia trzy etapy.
```
[A] -> [B] -> [C]
```

* A to producent który otrzymuję żadania - liczbę zdażeń, danych którą konsumenci mogą obsłużyć, oraz generuje te dane.
 * B to producent-konsument, który przekazuję żądania do A, oraz przetważa otrzymane od producenta dane i wysyła do konsumenta
 * C to konsument, który otrzymaje dane i przetważa je w zdefiniowany przez programistę sposób

 ### Producent

W producencie najważniejsze są dwie funkcje:
*  ```init/1``` która inicjalizuje stan początkowy. Zwraca dwu-elementową tuple, w której pierwszy element określa moduł jako producenta, a drugi element to stan początkowy, na przykład jakieś źródło danych przekazane w argumencie ```arg```.

* ```handle_demand/2``` odpowiada za obsługę żadania konsumenta odnosnie danych. Argument ```demand``` określa maksymalną liczbę danych jaką konsument może przetworzyć i to na jego podstawie producent generuje odpowiednią liczbę danych i uaktualnia swój stan. Zwraca tuple trzy-elementową, w której pierwszy element to atom ```:noreply```, drugi to lista danych dla konsumenta, a trzeci nowy stan producenta.

 ```elixir
  defmodule Producer do
      use GenStage

      def init(arg) do
        {:producer, :initial_state}
      end

      def handle_demand(demand, state) do
        {:noreply, [:data, :data], state}
      end
    end
 ```

### Producent-Konsument

W producencie-konsumencie musimy zwrócić uwage na:
* ```init/1``` działa dokładnie tak samo jak dla producenta tylko, musimy w pierwszym elemencie tupli zaznaczyć, że chodzi o producenta-konsumenta

* ```handle_events/3``` obsługuję dane otrzymywane od producenta i przekazuję je dalej. Również zwraca trzy-elementową tuplę, taką samą jak ```handle_demands``` w przypadku producenta.

```elixir
 defmodule ProducerConsumer do
      use GenStage

      def init(arg) do
        {:producer_consumer, :initial_state}
      end

      def handle_events(data, from, state) do
        {:noreply, data, state}
      end
    end
```

### Konsument
 W konsumencie wyrówniamy te same funkcję co w przypadku producenta-konsumenta z dwoma różnicami. W przypadku konsumenta ```handle_events/3``` musi zwracać tuplę, w której drugi element to pusta lista, natomiast w funkcji ```init/1``` musimy zaznaczyć, że chodzi o konsumenta w zwracanej przez nas tupli.

```elixir
defmodule Consumer do
      use GenStage  

      def init(arg) do
        {:consumer, :initial_state}
      end

      def handle_events(data, from, state) do
        {:noreply, [], state}
      end
    end
```

### Stosowanie

Schemat jest bardzo prosty, jednak pozwala nam tworzyć również bardziej skomplikowane projekty, w których może pracować wielu producentów i konsumentów. Ogólnie GenStage świetnie nadaje się do tworzenia:

* Potoków transformacji danych
* Kolejek
* Obsług zdarzeń

<p align=center>
  <img src=images/flow.png/>
</p>


## Demo

W celu przetestowania możliwości GenStage w praktyce stworzyłem mini-projekt z użyciem Elixir GenStage.

### Opis projektu

Celem projektu było zademonstrować sekwencyjne przetwarzanie danych przez GenStage. Postanowiłem więc przetwarzać cytaty dotyczącę programowania. Są one źródłem danych dla producenta, który wysyła je do konsumentów. Następnie cytaty są analizowane pod względem sentymentu. Na koniec można ustawić odpowiedni próg minimalny sentymentu, i program odfiltruje te z mniejszą wartością, a pozostałe zapisze do pliku. Pomysł można swobodnie rozszerzać o kolejnych konsumentów, w celu dalszego przetwarzania cytatów.

### Implementacja

##### Producent

Zadaniem producenta jest zaspokajać żądania konsumentów, danymi, w tym wypadku cytatami. Skorzystałem z API, które dostarcza losowe cytaty programistyczne. W celu wysyłania request-ów skorzystałem z Tesli, natomiast do przetworzonia JSON-a, wykorzystałem Poison. Parametr max_number określa maksymalną liczbę cytatów jakie chcemy przetworzyć.

```elixir
defmodule Producer do
  use GenStage

  def start_link(max_number) do
    GenStage.start_link(Producer, max_number, name: Producer)
  end

  def init(max_number) do
    {:producer, max_number}
  end

  def handle_demand(demand, state)  when demand>0 and state>=demand do
    {:noreply, ProgrammingQuotes.get_quotes(demand), state - demand}
  end
  def handle_demand(_, state) do {:noreply, [], state} end

end

defmodule ProgrammingQuotes do

  def get_quote() do
    {:ok, response} = Tesla.get("https://programming-quotes-api.herokuapp.com/quotes/random")
    Poison.decode!(response.body) |>  Map.delete("_id") |> Map.delete("id")
  end

  def get_quotes(0) do [] end
  def get_quotes(number) do
    [get_quote()] ++ get_quotes(number-1)
  end
end
```

##### Producent-Konsument

Zadaniem producenta-konsumenta jest wykonywanie analizy sentymentu cytatu. W tym wypadku również skorzystałem z API, które analizuje tekst właśnie pod takim kątem. 

```elixir
defmodule ProducerConsumer do
  use GenStage

  def start_link do
    GenStage.start_link(ProducerConsumer, :some_state, name: ProducerConsumer)
  end

  def init(initial) do
    {:producer_consumer, initial, subscribe_to: [{Producer, max_demand: 3}]}
  end

  def handle_events(data, _, state) do
    {:noreply,analyse(data), state}
  end

  def analyse(data) do
    data |> Enum.map(&(Map.put(&1, "result", sentiment_analysis(&1["en"]))))
  end

  def sentiment_analysis(text) do
    body = Poison.encode!(%{"text" => text})
    {:ok, response} = Tesla.post("https://sentim-api.herokuapp.com/api/v1/", body, headers: [{"content-type", "application/json"}])
    Poison.decode!(response.body) |>  Map.get("result")
  end

end
```

##### Konsument

Na samym końcu konsument filtruje cytaty, których wartość sentymentu jest niższa niż ustalony próg - parametr threshold. Po odfiltrowaniu konsument zapisuje przetworzony cytaty w pliku result.txt

```elixir
defmodule Consumer do
  use GenStage

  def start_link(threshold) do
    GenStage.start_link(Consumer, threshold)
  end

  def init(threshold) do
    {:ok, file} = File.open("result.txt", [:write, :utf8])
    {:consumer, %{:threshold => threshold, :file => file}, subscribe_to: [{ProducerConsumer, max_demand: 3}]}
  end

  def handle_events(data, _, state) do
    Enum.filter(data, &(&1["result"]["polarity"] > state.threshold)) |> Enum.each(&(save_to_file(state.file, &1)))
    {:noreply, [], state}
  end

  def save_to_file(file, result) do
    IO.write(file, "Autor: #{result["author"]}\n")
    IO.write(file, "Cytat: #{result["en"]}\n")
    IO.write(file, "Polaryzacja sentymentu: #{result["result"]["polarity"]}\n")
    IO.write(file, "***********************************************\n")
  end
end
```

##### test

Na koniec przetestowałem działanie programu dla następujących parametrów:
* max_number = 100
* threshold = 0.1

W pliku result.txt, mogłem znaleźć przetworzone cytaty, oto przykładowo 4 pierwsze:
```
Autor: Linus Torvalds
Cytat: See, you not only have to be a good coder to create a system like Linux, you have to be a sneaky bastard too ;-)
Polaryzacja sentymentu: 0.32
***********************************************
Autor: Ken Thompson
Cytat: One of my most productive days was throwing away 1,000 lines of code.
Polaryzacja sentymentu: 0.5
***********************************************
Autor: Simon Peyton Jones
Cytat: I characterize functional programming as a radical and elegant attack on the whole enterprise of writing programs.
Polaryzacja sentymentu: 0.35
***********************************************
Autor: Linus Torvalds
Cytat: If you need more than 3 levels of indentation, you're screwed anyway, and should fix your program.
Polaryzacja sentymentu: 0.5
***********************************************
.
.
.
```

## Podsumowanie

Elixir GenStage jest idealnym narzędzięm do sekwencyjnego przetwarzania danych i oceniam go bardzo pozytywnie. Nie jest to co prawda bardzo popularne narzędzię ale wydaje się bardzo przydatne, szczególnie do przetwarzania jakichś wiekszych ilości danych bądź zdarzeń.

<p align=center>
  <img src=images/trends.png/>
</p>
