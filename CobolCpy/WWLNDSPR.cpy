001000*** EDIT ALLOWED              ***********************************         
020000*                             **  ANVÄNDS VID KONVERTERING MELLAN         
030000*                             **  LANDSKOD (IDLANDX2) TILL                
030100*                             **  SPRÅKKOD (IDSPRAK)                      
030110*                             **  LANDSKOD + SPRÅKKOD (ISO STD)           
030200*                                                                         
042000*  OBS! LÄGG TILL NYA LÄNDER I BOKSTAVSORDNING I 'VALUE'                  
042100*       OCH ANPASSA OCCURS NEDERST!                                       
043000*                                                                         
044000 01  WWLNDSPR.                                                            
070000     03 WWLNDSPR-TABELL.                                                  
071000        05 AUSTRIA PIC X(11) VALUE 'AU=DE=de_AT'.                         
071100        05 BELGIUM PIC X(11) VALUE 'BE=NL=nl_BE'.                         
071200        05 SWISS   PIC X(11) VALUE 'CH=DE=de_CH'.                         
071300        05 GERMANY PIC X(11) VALUE 'DE=DE=de_DE'.                         
071400        05 SPAIN   PIC X(11) VALUE 'ES=ES=es_ES'.                         
071500        05 FINLAND PIC X(11) VALUE 'FI=FI=fi_FI'.                         
071600        05 FRANCE  PIC X(11) VALUE 'FR=FR=fr_FR'.                         
072000        05 ENGLAND PIC X(11) VALUE 'GB=GB=en_GB'.                         
072400        05 ITALY   PIC X(11) VALUE 'IT=IT=it_IT'.                         
072600        05 HOLLAND PIC X(11) VALUE 'NL=NL=nl_NL'.                         
072701        05 NORWAY  PIC X(11) VALUE 'NO=NO=no_NO'.                         
073300        05 POLAND  PIC X(11) VALUE 'PL=PL=pl_PL'.                         
073500        05 SWEDEN  PIC X(11) VALUE 'SE=SE=sv_SE'.                         
190000                                                                          
280000     03 FILLER REDEFINES WWLNDSPR-TABELL.                                 
290000        05 WWLNDSPR-RAD      OCCURS 13 TIMES                              
300000                       ASCENDING KEY IS WWLNDSPR-IDLANDX2                 
301000                       INDEXED BY SPR-IX.                                 
320000           07 WWLNDSPR-IDLANDX2       PIC X(2).                           
321000           07 FILLER                  PIC X(1).                           
330000           07 WWLNDSPR-IDSPRAK        PIC X(2).                           
330100           07 FILLER                  PIC X(1).                           
330200           07 WWLNDSPR-IDLANDX2-IDSPRAK-GRP                               
330300                                      PIC X(5).                           
341000*                                                                         
