-module(my_life).
-export ([my_life/1]).

-record(place, {id, moments}).
-record(moment, {id, friends, lovers, meaning}).
-record(person, {id, love_level}).

-author(john).
-author(paul).
-author(george).
-author(ringo).

my_life(NewPlaces) ->
  Places = db:get_all(places),
  UpdatedPlaces = [NewPlace || NewPlace <- NewPlaces, member(NewPlace, Places)],
  lists:foreach(fun(Place) -> db:insert(places, Place) end, UpdatedPlaces),
  DeletedPlaces = [Place || Place <- Places, not member(Place, NewPlaces)],
  Moments = [Moment || Place <- Places, Moment <- Places#place.moments],
  People =
    [Person || Moment <- Moments, 
               Person <- Moment#moment.lovers ++ Moment#moment.friends],
  {Dead, Living} = lists:partition(fun person:is_dead/1, People),
  lists:foreach(fun person:love/1, Dead ++ Living),

  [You | _Rest] = lists:keysort(#person.love_level, People),

  UpdatedMemories = [ Moment#moment{meaning = null} || Moment <- Moments],

  lists:foreach(
    fun(Person) ->
      timer:sleep(random:uniform(100) + 100),
      case Person of
        You -> person:love(You);
        Other -> person:think_about(Other)
      end
    end, People).

member(Place, Places) -> lists:keymember(Place#place.id, #place.id, Places).
