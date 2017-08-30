module ReplyCodes(
    rpl_welcome, 
    rpl_yourhost, 
    rpl_created, 
    rpl_myinfo, 
    rpl_luserclient, 
    rpl_luserop, 
    rpl_luserunknown, 
    rpl_luserchannels, 
    rpl_luserme, 
    rpl_away, 
    rpl_unaway, 
    rpl_nowaway, 
    rpl_whoisuser, 
    rpl_whoisserver, 
    rpl_whoisoperator, 
    rpl_whoisidle, 
    rpl_endofwhois, 
    rpl_whoischannels, 
    rpl_whoreply, 
    rpl_endofwho, 
    rpl_list, 
    rpl_listend, 
    rpl_channelmodeis, 
    rpl_notopic, 
    rpl_topic, 
    rpl_namreply, 
    rpl_endofnames, 
    rpl_motdstart, 
    rpl_motd, 
    rpl_endofmotd, 
    rpl_youreoper, 
    err_nosuchnick,
    err_nosuchchannel,
    err_cannotsendtochan,
    err_unknowncommand,
    err_nomotd,
    err_nonicknamegiven,
    err_nicknameinuse,
    err_usernotinchannel,
    err_notonchannel,
    err_notregistered,
    err_needmoreparams,
    err_alreadyregistred,
    err_passwdmismatch,
    err_unknownmode,
    err_chanoprivsneeded,
    err_umodeunknownflag,
    err_usersdontmatch
  ) where

rpl_welcome :: String
rpl_welcome = "001"

rpl_yourhost :: String
rpl_yourhost = "002"

rpl_created :: String
rpl_created = "003"

rpl_myinfo :: String
rpl_myinfo = "004"

rpl_luserclient :: String
rpl_luserclient = "251"

rpl_luserop :: String
rpl_luserop = "252"

rpl_luserunknown :: String
rpl_luserunknown = "253"

rpl_luserchannels :: String
rpl_luserchannels = "254"

rpl_luserme :: String
rpl_luserme = "255"

rpl_away :: String
rpl_away = "301"

rpl_unaway :: String
rpl_unaway = "305"

rpl_nowaway :: String
rpl_nowaway = "306"

rpl_whoisuser :: String
rpl_whoisuser = "311"

rpl_whoisserver :: String
rpl_whoisserver = "312"

rpl_whoisoperator :: String
rpl_whoisoperator = "313"

rpl_whoisidle :: String
rpl_whoisidle = "317"

rpl_endofwhois :: String
rpl_endofwhois = "318"

rpl_whoischannels :: String
rpl_whoischannels = "319"

rpl_whoreply :: String
rpl_whoreply = "352"

rpl_endofwho :: String
rpl_endofwho = "315"

rpl_list :: String
rpl_list = "322"

rpl_listend :: String
rpl_listend = "323"

rpl_channelmodeis :: String
rpl_channelmodeis = "324"

rpl_notopic :: String
rpl_notopic = "331"

rpl_topic :: String
rpl_topic = "332"

rpl_namreply :: String
rpl_namreply = "353"

rpl_endofnames :: String
rpl_endofnames = "366"

rpl_motdstart :: String
rpl_motdstart = "375"

rpl_motd :: String
rpl_motd = "372"

rpl_endofmotd :: String
rpl_endofmotd = "376"

rpl_youreoper :: String
rpl_youreoper = "381"

err_nosuchnick :: String
err_nosuchnick = "401"

err_nosuchchannel :: String
err_nosuchchannel = "403"

err_cannotsendtochan :: String
err_cannotsendtochan = "404"

err_unknowncommand :: String
err_unknowncommand = "421"

err_nomotd :: String
err_nomotd = "422"

err_nonicknamegiven :: String
err_nonicknamegiven = "431"

err_nicknameinuse :: String
err_nicknameinuse = "433"

err_usernotinchannel :: String
err_usernotinchannel = "441"

err_notonchannel :: String
err_notonchannel = "442"

err_notregistered :: String
err_notregistered = "451"

err_needmoreparams :: String
err_needmoreparams = "461"

err_alreadyregistred :: String
err_alreadyregistred = "462"

err_passwdmismatch :: String
err_passwdmismatch = "464"

err_unknownmode :: String
err_unknownmode = "472"

err_chanoprivsneeded :: String
err_chanoprivsneeded = "482"

err_umodeunknownflag :: String
err_umodeunknownflag = "501"

err_usersdontmatch :: String
err_usersdontmatch = "502"
