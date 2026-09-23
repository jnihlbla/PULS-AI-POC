000100 01  REQU-W60362I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6036200              
000300*                                                                         
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 REQU-IDTEKINF-KEY    PIC X(360).                                  
001100*                                 TEKNINSK INFO ARTIKEL ETIKETT           
001200*                                 PART LABEL TECHNICAL INFO               
001300     03 REQU-FLVISA-KEY      PIC X.                                       
001400*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
001500*                                 GENERAL FLAG FOR SHOWING INFO           
001600     03 REQU-IDARTNR-UPD     PIC 9(8).                                    
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 REQU-IDTEKINF-UPD    PIC X(360).                                  
002000*                                 TEKNINSK INFO ARTIKEL ETIKETT           
002100*                                 PART LABEL TECHNICAL INFO               
002200     03 REQU-KVRADER         PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400*                                 NUMBER OF LINES                         
002500     03 REQU-LINE            OCCURS 999 TIMES.                            
002600        05 REQU-KDCMD-LINE   PIC X.                                       
002700*                                 RAD-UPPDATERINGSKOMMANDO                
002800*                                  BLANK  = INGENTING                     
002900*                                  D , B  = DELETE                        
003000*                                  R , Ä  = REPLACE                       
003100*                                  I,N,A  = INSERT                        
003200*                                  S , V  = SELECT                        
003300*                                  P , P  = PRINT                         
003400*                                  C , K  = COPY                          
003500*                                 LINE UPDATE COMMAND                     
003600        05 REQU-IDARTNR-LINE PIC 9(8).                                    
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900        05 REQU-IDTEKINF-LINE                                             
004000                             PIC X(360).                                  
004100*                                 TEKNINSK INFO ARTIKEL ETIKETT           
004200*                                 PART LABEL TECHNICAL INFO               
004300*** END OF VILMAII-COPY LENGTH= 369375 BYTES                              
