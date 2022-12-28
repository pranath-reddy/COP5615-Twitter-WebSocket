%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-2
%% Mock Twitter-Engine Websockets

%% Client JSON Commands Guide
%% Register: {"action":"register", "username":"user1", "password":"password123"}
%% Login: {"action":"login", "username":"user1", "password":"password123"}
%% Tweet: {"action":"tweet", "username":"user1", "tweet":"hello #hashtag"}
%% Retweet: {"action":"retweet", "username":"user1"}
%% Subscribe: {"action":"subscribe", "username":"user2", "subscribe_to":"user1"}
%% Hashtag Query: {"action":"hashQuery", "username":"user1", "query":"#hashtag"}
%% Sub Query: {"action":"subQuery", "username":"user1", "query":"SubTweets", "keyword":"hello"}
%% Mention Query: {"action":"mentionQuery", "username":"user1", "query":"Mentions"}
%% Logoff: {"action":"logoff", "username":"user1", "password":"password123"}
%% Bonus: {"action":"bonus", "username":"user1", "path":"rsa.pem"}

-module(server_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-import(pubKeyAuth, [authenticate/2]).

init(Req0, State) ->
    % Args:
    % Req: Information about the request
    % State: state
    % Adjust idle_timeout based on requirements

    {cowboy_websocket, Req0, State, #{idle_timeout => 30000000}}.

websocket_init(State) ->
  % Args:
  % State: state

  io:fwrite("Connected a Client!\n"),
  initiate_tables(),
  {[{text, <<"Connected! Welcome to Twitter!">>}], State}.

websocket_handle({text, JObj}, State) ->
  % Args:
  % JObj: JSON object received from client
  % State: state

  io:fwrite("Handle Incoming!\n"),
	io:format("Recieved JSON Object: ~p\n", [JObj]),
  {struct, JPObj} = mochijson2:decode(JObj),
  io:format("Decoded Object: ~p ~n", [JPObj]),
  Action = binary_to_list(proplists:get_value(<<"action">>, JPObj)),
  io:format("Recieved Action: ~p\n", [Action]),
	Msg = "Recieved Action from Client - " ++ Action,
	send_message(Msg),

  %% Process Actions
  if

    (Action =:= "register") ->
      io:fwrite("Recieved Registration Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Password = binary_to_list(proplists:get_value(<<"password">>, JPObj)),
      io:format("The username received is: ~p\n", [Username]),
      send_message("Registration Request Accepted"),
      Reg_Result = registration_service(Username, Password),
      io:format("Registration Result: ~p\n", [Reg_Result]),
      send_message(Reg_Result),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "login") ->
      io:fwrite("Recieved Login Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Password = binary_to_list(proplists:get_value(<<"password">>, JPObj)),
      io:format("The username received is: ~p\n", [Username]),
      send_message("Login Request Accepted"),
      Reg_Result = login_service(Username, Password),
      io:format("Login Result: ~p\n", [Reg_Result]),
      send_message(Reg_Result),
      io:format("Sending Complete Signal\n"),
      if
        Reg_Result == "Success: User Logged In Successfully" ->
          send_message("Welcome");
        true ->
          send_message("Done")
      end,
      io:fwrite("Done\n");

    (Action =:= "tweet") ->
      io:fwrite("Recieved Tweet from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Tweet = binary_to_list(proplists:get_value(<<"tweet">>, JPObj)),
      io:fwrite("\n*********************************\n"),
      io:format("The user ~p tweets: \n", [Username]),
      io:format("~p\n", [Tweet]),
      io:fwrite("*********************************\n\n"),
      tweet_service(Username, Tweet, "Tweet"),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "retweet") ->
      io:fwrite("Recieved Retweet Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      retweet_service(Username),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "hashQuery") ->
      io:fwrite("Recieved Query for hashtags from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Query = binary_to_list(proplists:get_value(<<"query">>, JPObj)),
      query_service(Username, Query, "None"),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "mentionQuery") ->
      io:fwrite("Recieved Query for mentions from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Query = binary_to_list(proplists:get_value(<<"query">>, JPObj)),
      query_service(Username, Query, "None"),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "subQuery") ->
      io:fwrite("Recieved Query for Tweets Subscribed to\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Query = binary_to_list(proplists:get_value(<<"query">>, JPObj)),
      Key = binary_to_list(proplists:get_value(<<"keyword">>, JPObj)),
      query_service(Username, Query, Key),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "subscribe") ->
      io:fwrite("Subscription Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      SUsername = binary_to_list(proplists:get_value(<<"subscribe_to">>, JPObj)),
      io:format("The user ~p wants to subscribe to: \n", [Username]),
      io:format("~p\n", [SUsername]),
      Sub_Result = subscription_service(Username, SUsername),
      io:format("Sending Complete Signal\n"),
      send_message(Sub_Result),
      io:fwrite("Done\n");

    (Action =:= "logoff") ->
      io:fwrite("Recieved Logoff Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Password = binary_to_list(proplists:get_value(<<"password">>, JPObj)),
      io:format("The username received is: ~p\n", [Username]),
      send_message("Logoff Request Accepted"),
      Reg_Result = logoff_service(Username, Password),
      io:format("Logoff Result: ~p\n", [Reg_Result]),
      send_message(Reg_Result),
      io:format("Sending Complete Signal\n"),
      send_message("Done!"),
      io:fwrite("Done\n");

    (Action =:= "bonus") ->
      io:fwrite("Recieved Authentication Request from User\n"),
      Username = binary_to_list(proplists:get_value(<<"username">>, JPObj)),
      Path = binary_to_list(proplists:get_value(<<"path">>, JPObj)),
      io:format("The username received is: ~p\n", [Username]),
      io:format("The path received is: ~p\n", [Path]),
      send_message("Authentication Request Accepted"),
      {_Date, Time} = calendar:universal_time(),
      Sec = calendar:time_to_seconds(Time),
      Reg_Result = authenticate(Path, Sec),
      if
        Reg_Result == true ->
          send_message("Authentication Successfull! Welcome");
        true ->
          send_message("Authentication Failed")
      end,
      io:fwrite("Done\n");

    true ->
      io:fwrite("Invalid Action\n"),
      send_message("Invalid Action, Please Try Again!")

  end,
	{reply, {text, <<"Action Recieved and Processed!">>}, State}.

% Send messages to client
send_message(Msg) ->
  % Args:
  % Msg: Message to send

	Self = self(),
	Self ! Msg,
	io:fwrite("Message sent to client!\n").

websocket_info(_Info, State) ->
  % Args:
  % Info: Message to parse
  % State: state

  io:format("Message to send: ~p\n", [_Info]),
  Result = mochijson:encode({struct, [{twitter_update, _Info}]}),
	io:format("Sending: ~p\n", [Result]),
	{[{text, Result}], State}.

% initiate ets tables
initiate_tables() ->
  % Args:
  % None

  ets:new(accounts, [set, named_table, public]),
  ets:new(tweets, [bag, named_table, public]),
  ets:new(subscribers, [bag, named_table, public]),
  ets:new(hashtags, [bag, named_table, public]),
  ets:new(mymentions, [bag, named_table, public]).

% Registers user by adding to ets table
registration_service(Username, Password)->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)

  io:fwrite("Registering User\n"),
  Status_0 = "Fail: User ID Already Exists",
  Status_1 = "Success: User Registered Successfully. Please Login to use Twitter!",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
        Status_0;
    true ->
        ets:insert(accounts, {Username, Password, "offline"}),
        Status_1
  end.

% Logs in user by changing status in ets table
login_service(Username, Password) ->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)

  io:fwrite("Logging in User\n"),
  Status_0_1 = "Fail: Invalid Credentials, Check Password",
  Status_0_2 = "Fail: User Account Does Not Exist",
  Status_1 = "Success: User Logged In Successfully",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
      {_, PWD, _} = lists:nth(1, Out),
      if
        PWD == Password ->
         ets:insert(accounts, {Username, Password, "online"}),
         Status_1;
       true ->
         Status_0_1
      end;
    true ->
      Status_0_2
  end.

% Distribute tweet to subsribers
distribute(Message, Username) ->
  % Args:
  % Message to be passed
  % Username: Username (ID)

  io:fwrite("Distribute Called\n"),
  ets:insert(tweets, {Username, Message}),
  hashtag_service(Username, Message),
  send_message(Message).

% Store and distribute tweets
tweet_service(Username, Tweet, Mode) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet
  % Mode: tweet or retweet

  ets:insert(tweets, {Username, Tweet}),
  hashtag_service(Username, Tweet),
  mention_service(Username, Tweet),
  io:fwrite("Distributing Tweet\n"),
  %if
  %  Mode == "Tweet" ->
  %    TMessage = Username ++ " tweeted: " ++ Tweet;
  %  true ->
  %    TMessage = Username ++ " retweeted: " ++ Tweet
  %end,
  TMessage = case Mode of
    "Tweet" -> Username ++ " tweeted: " ++ Tweet;
    "Retweet" -> Username ++ " retweeted: " ++ Tweet
  end,
  io:fwrite("Message to be distributed ~p\n", [TMessage]),
  send_message(TMessage),
  Sub_list = ets:lookup(subscribers, Username),
  io:fwrite("Subscribers Info: ~p\n", [Sub_list]),
  if
    length(Sub_list) > 0 ->
      _ = [distribute(TMessage, U) || {SU, U} <- Sub_list, SU == Username],
      io:fwrite("Tweet Distributed to Subscribers\n");
    true ->
      io:fwrite("No Subscribers Found\n")
  end.

% Process and check tweets for hashtags and store in table
hashtag_service(Username, Tweet) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet

  H = string:find(Tweet, "#"),
  if
    H == nomatch ->
      io:fwrite("Tweet has no hashtag\n");
    true ->
      Hashtag = hd(string:split(H, " ")),
      ets:insert(hashtags, {Hashtag, Username, Tweet}),
      io:fwrite("Tweet has hashtag: ~p\n", [Hashtag])
  end.

% Process user queries
query_service(Username, Query, Key) ->
  % Args:
  % Username: Username (ID)
  % Query: Mention or Hashtag

  if
    Query == "Mentions" ->
      Mention = "@" ++ Username,
      Tweet_list = ets:lookup(mymentions, Mention),
      Twt_list = [Message || {_, _, Message} <- Tweet_list],
      Result = string:join(Twt_list, " | "),
      send_message(Result),
      io:fwrite("Mentioned tweets: ~p\n", [Result]);
    Query == "SubTweets" ->
      Tweet_list = ets:lookup(tweets, Username),
      Twt_list = [Message || {_, Message} <- Tweet_list, string:find(Message, Key) /= nomatch],
      Result = string:join(Twt_list, " | "),
      send_message(Result),
      io:fwrite("Subscribed tweets with keyword: ~p\n", [Result]);
    true ->
      Hashtag = hd(string:split(string:find(Query, "#"), " ")),
      io:fwrite("Recieved hashtag query: ~p\n", [Hashtag]),
      Hash_list = ets:lookup(hashtags, Hashtag),
      Hs_list = [Message || {_, _, Message} <- Hash_list],
      Result = string:join(Hs_list, " | "),
      send_message(Result),
      io:fwrite("Hashtag tweets: ~p\n", [Result])
  end.

% Process and check tweets for mentions and store in table
% Deliver tweets live to online mentioned users
mention_service(Username, Tweet) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet

  M = string:find(Tweet, "@"),
  if
    M == nomatch ->
      io:fwrite("Tweet has no mentions\n");
    true ->
      Mention = hd(string:split(M, " ")),
      io:fwrite("Tweet has mention: ~p\n", [Mention]),

      Message = Username ++ " mentioned: " ++ Tweet,
      MUsername = string:slice(Mention, 1),
      Out = ets:lookup(accounts, MUsername),
      ets:insert(mymentions, {Mention, MUsername, Message}),
      io:fwrite("Mentioned user info in table: ~p\n", [Out]),
      send_message(Message)
  end.

% retweet latest tweet in the feed of the user
retweet_service(Username) ->
  % Args:
  % Username: Username (ID)

  io:fwrite("Retweet!\n"),
  Tweet_list = ets:lookup(tweets, Username),
  Latest_Tweet = lists:last(Tweet_list),
  io:fwrite("Latest Tweet: ~p\n", [Latest_Tweet]),
  {ID, Twt} = Latest_Tweet,
  tweet_service(ID, Twt, "Retweet").

% Update subscribers table
subscription_service(Username, SUsername) ->
  % Args:
  % Username: Subscriber Username
  % SUsername: Main Username (To Sub to)

  Status_0 = "Fail: Account Does Not Exist",
  Status_1 = "Success: User Subscribed",
  Out = ets:lookup(accounts, SUsername),
  if
    length(Out) > 0 ->
      io:fwrite("Adding Subscriber\n"),
      ets:insert(subscribers, {SUsername, Username}),
      io:fwrite("Subscriber Added\n"),
      Message = Username ++ " now follows " ++ SUsername,
      send_message(Message),
      Status_1;
    true ->
      Status_0
  end.

% Logs off user by changing status in ets table
logoff_service(Username, Password) ->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)

  io:fwrite("Logging off User\n"),
  Status_0 = "Fail: User Account Does Not Exist",
  Status_1 = "Success: User Logged Off Successfully",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
      ets:insert(accounts, {Username, Password, "offline"}),
      Status_1;
    true ->
      Status_0
  end.
