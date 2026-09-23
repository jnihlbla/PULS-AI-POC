000100 01  REQU-WZ0410I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WZ0410             
000300*                                 RESTART DISTRIBUTION LOCATE             
000400     03 REQU-IDOUTDEST-KEY   PIC X(60).                                   
000500*                                 FYSISK OUTPUT DESTINATION               
000600*                                 PHYSICAL OUTPUT DESTINATION             
000700     03 REQU-TIREGDAT-KEY    PIC X(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 REQU-TIKLOCK-KEY     PIC X(8).                                    
001100*                                 KLOCKSLAG (TTMMSSTH)                    
001200*                                 TIME OF DAY (HHMMSSTH)                  
001300     03 REQU-IDLIST-KEY      PIC X(10).                                   
001400*                                 LISTIDENTITET                           
001500*                                 LIST IDENTITY                           
001600     03 REQU-IDOUTREC-KEY    PIC X(30).                                   
001700*                                 OUTPUTMOTTAGARE                         
001800*                                 OUTPUT RECEIVER                         
001900     03 REQU-FLRULEMISS-KEY  PIC X.                                       
002000*                                 REGLER SAKNAS                           
002100*                                 RULES ARE MISSING                       
002200     03 REQU-IDOUTTYPE-KEY   PIC X(15).                                   
002300*                                 OUTPUTTYP                               
002400*                                 OUTPUT TYPE                             
002500     03 REQU-KDOUTMETH-KEY   PIC X(4).                                    
002600*                                 OUTPUTMETOD                             
002700*                                 OUTPUT METHOD                           
002800     03 REQU-KVRADER         PIC 9(5).                                    
002900*                                 ANTAL RADER                             
003000*                                 NUMBER OF LINES                         
003100     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
003200*                                 GRUPP MED TABELLRADER                   
003300        05 REQU-KDCMD-LINE   PIC X.                                       
003400*                                 RAD-UPPDATERINGSKOMMANDO                
003500*                                  BLANK  = INGENTING                     
003600*                                  D , B  = DELETE                        
003700*                                  R , Ä  = REPLACE                       
003800*                                  I,N,A  = INSERT                        
003900*                                  S , V  = SELECT                        
004000*                                  P , P  = PRINT                         
004100*                                  C , K  = COPY                          
004200*                                 LINE UPDATE COMMAND                     
004300        05 REQU-TIREGDAT-LINE                                             
004400                             PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600*                                 REGISTRATION DATE (YYMMDD)              
004700        05 REQU-TIKLOCK-LINE PIC 9(8).                                    
004800*                                 KLOCKSLAG (TTMMSSTH)                    
004900*                                 TIME OF DAY (HHMMSSTH)                  
005000        05 REQU-IDLOPNR-LINE PIC 9(3).                                    
005100*                                 LÖPNUMMER                               
005200*                                 SEQUENCE NUMBER                         
005300        05 REQU-IDOUTREC-LINE                                             
005400                             PIC X(30).                                   
005500*                                 OUTPUTMOTTAGARE                         
005600*                                 OUTPUT RECEIVER                         
005700        05 REQU-IDOUTTYPE-LINE                                            
005800                             PIC X(15).                                   
005900*                                 OUTPUTTYP                               
006000*                                 OUTPUT TYPE                             
006100        05 REQU-IDLIST-LINE  PIC X(10).                                   
006200*                                 LISTIDENTITET                           
006300*                                 LIST IDENTITY                           
006400*** END OF VILMAII-COPY LENGTH= 36639 BYTES                               
