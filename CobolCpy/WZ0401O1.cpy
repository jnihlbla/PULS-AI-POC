000100 01  RESP-WZ0401O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WZ0401         
000300*                                 DISTRIBUTION RULES LOCATE               
000400     03 RESP-IDOUTTYPE-KEY   PIC X(15).                                   
000500*                                 OUTPUTTYP                               
000600*                                 OUTPUT TYPE                             
000700     03 RESP-IDOUTREC-KEY    PIC X(30).                                   
000800*                                 OUTPUTMOTTAGARE                         
000900*                                 OUTPUT RECEIVER                         
001000     03 RESP-IDOUTDEST-KEY   PIC X(60).                                   
001100*                                 FYSISK OUTPUT DESTINATION               
001200*                                 PHYSICAL OUTPUT DESTINATION             
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-TABELLRAD       OCCURS 999 TIMES.                            
001700*                                 GRUPP MED TABELLRADER                   
001800        05 RESP-IDOUTTYPE-LINE                                            
001900                             PIC X(15).                                   
002000*                                 OUTPUTTYP                               
002100*                                 OUTPUT TYPE                             
002200        05 RESP-IDOUTREC-FROM-LINE                                        
002300                             PIC X(30).                                   
002400*                                 OUTPUTMOTTAGARE (FR.O.M)                
002500*                                 OUTPUT RECEIVER (FROM)                  
002600        05 RESP-IDOUTREC-TO-LINE                                          
002700                             PIC X(30).                                   
002800*                                 OUTPUTMOTTAGARE (T.O.M)                 
002900*                                 OUTPUT RECEIVER (TO)                    
003000        05 RESP-TIREGDAT-LINE                                             
003100                             PIC 9(6).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400        05 RESP-TIUPPDAT-LINE                                             
003500                             PIC 9(6).                                    
003600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003700*                                 UPDATING DATE     (YYMMDD)              
003800        05 RESP-TIANVDAT-LINE                                             
003900                             PIC 9(6).                                    
004000*                                 SENAST-ANVÄND DATUM    (ÅÅMMDD)         
004100*                                 DATE OF LAST USAGE     (YYMMDD)         
004200        05 RESP-IDUSER-LINE  PIC X(8).                                    
004300*                                 ANVÄNDARENS SÄKERHETS ID                
004400*                                 USER SECURITY-IDENTITY                  
004500*** END OF VILMAII-COPY LENGTH= 101009 BYTES                              
