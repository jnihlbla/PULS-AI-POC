000100 01  RESP-WZ0410O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WZ0410         
000300*                                 RESTART DISTRIBUTION LOCATE             
000400     03 RESP-IDOUTDEST-KEY   PIC X(60).                                   
000500*                                 FYSISK OUTPUT DESTINATION               
000600*                                 PHYSICAL OUTPUT DESTINATION             
000700     03 RESP-TIREGDAT-KEY    PIC X(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 RESP-TIKLOCK-KEY     PIC X(8).                                    
001100*                                 KLOCKSLAG (TTMMSSTH)                    
001200*                                 TIME OF DAY (HHMMSSTH)                  
001300     03 RESP-IDLIST-KEY      PIC X(10).                                   
001400*                                 LISTIDENTITET                           
001500*                                 LIST IDENTITY                           
001600     03 RESP-IDOUTREC-KEY    PIC X(30).                                   
001700*                                 OUTPUTMOTTAGARE                         
001800*                                 OUTPUT RECEIVER                         
001900     03 RESP-FLRULEMISS-KEY  PIC X.                                       
002000*                                 REGLER SAKNAS                           
002100*                                 RULES ARE MISSING                       
002200     03 RESP-IDOUTTYPE-KEY   PIC X(15).                                   
002300*                                 OUTPUTTYP                               
002400*                                 OUTPUT TYPE                             
002500     03 RESP-KDOUTMETH-KEY   PIC X(4).                                    
002600*                                 OUTPUTMETOD                             
002700*                                 OUTPUT METHOD                           
002800     03 RESP-KVRADER         PIC Z(4)9.                                   
002900*                                 ANTAL RADER                             
003000*                                 NUMBER OF LINES                         
003100     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
003200*                                 GRUPP MED TABELLRADER                   
003300        05 RESP-IDOUTDEST-LINE                                            
003400                             PIC X(60).                                   
003500*                                 FYSISK OUTPUT DESTINATION               
003600*                                 PHYSICAL OUTPUT DESTINATION             
003700        05 RESP-TIREGDAT-LINE                                             
003800                             PIC 9(6).                                    
003900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004000*                                 REGISTRATION DATE (YYMMDD)              
004100        05 RESP-TIKLOCK-LINE PIC 9(8).                                    
004200*                                 KLOCKSLAG (TTMMSSTH)                    
004300*                                 TIME OF DAY (HHMMSSTH)                  
004400        05 RESP-IDLOPNR-LINE PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600*                                 SEQUENCE NUMBER                         
004700        05 RESP-IDOUTREC-LINE                                             
004800                             PIC X(30).                                   
004900*                                 OUTPUTMOTTAGARE                         
005000*                                 OUTPUT RECEIVER                         
005100        05 RESP-KDOUTMETH-LINE                                            
005200                             PIC X(4).                                    
005300*                                 OUTPUTMETOD                             
005400*                                 OUTPUT METHOD                           
005500        05 RESP-IDOUTTYPE-LINE                                            
005600                             PIC X(15).                                   
005700*                                 OUTPUTTYP                               
005800*                                 OUTPUT TYPE                             
005900        05 RESP-IDLIST-LINE  PIC X(10).                                   
006000*                                 LISTIDENTITET                           
006100*                                 LIST IDENTITY                           
006200        05 RESP-KVANTEX-PRINTAD-LINE                                      
006300                             PIC 9.                                       
006400*                                 ANTAL GÅNGER LISTAN ÄR PRINTAD          
006500        05 RESP-TIAAMMDD-RENS-LINE                                        
006600                             PIC 9(6).                                    
006700*                                 RENSNINGSDATUM                          
006800*** END OF VILMAII-COPY LENGTH= 71639 BYTES                               
