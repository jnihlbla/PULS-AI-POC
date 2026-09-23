000100 01  REQU-W60115I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W60115                
000300*                                                                         
000400     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 REQU-IDFS-KEY        PIC X(8).                                    
000700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
000800     03 REQU-TIAVIDAT-KEY    PIC X(6).                                    
000900*                                 AVISERINGSDATUM (YYMMDD)                
001000     03 REQU-IDLBBET-KEY     PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200     03 REQU-ADINLOMR-PRT-KEY                                             
001300                             PIC X(4).                                    
001400*                                 PRINTERPLACERING                        
001500     03 REQU-IDDC-KEY        PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 REQU-IDLAND-SPR      PIC X(2).                                    
001800*                                 2-STÄLLIG LANDSKOD FÖR SPRÅK            
001900     03 REQU-IDTIDZON        PIC X(2).                                    
002000*                                 TIDZONER PÅ JORDEN.                     
002100     03 REQU-IDLEVNR-START   PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 REQU-IDFS-START      PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500     03 REQU-TIAVIDAT-START  PIC 9(6).                                    
002600*                                 AVISERINGSDATUM (YYMMDD)                
002700     03 REQU-IDLBBET-START   PIC X(12).                                   
002800*                                 LASTBÄRARBETECKNING                     
002900     03 REQU-OMSTART-INDX    PIC X(2).                                    
003000     03 REQU-INPUT.                                                       
003100*                                 INDATA FÖR UPPDATERING                  
003200        05 REQU-IDLBBET-UPD  PIC X(12).                                   
003300*                                 LASTBÄRARBETECKNING                     
003400        05 REQU-FLKLAR-UPD   PIC X.                                       
003500*                                 AVSLUTNINGSMARKERING                    
003600        05 REQU-ADINLOMR-LPL-UPD                                          
003700                             PIC X(4).                                    
003800*                                 LOSSNINGSPLATS                          
003900     03 REQU-KVUTSKR-AR      PIC X(3).                                    
004000     03 REQU-KVRADER         PIC X(5).                                    
004100*                                 ANTAL RADER                             
004200     03 REQU-KDCMDVAL-TAB    OCCURS 500 TIMES.                            
004300*                                 TABELL MED KDCMDVAL                     
004400        05 REQU-KDCMDVAL-LINE                                             
004500                             PIC X(3).                                    
004600*                                 GENERELL KOMMANDOKOD                    
004700        05 REQU-IDLEVNR-LINE PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900        05 REQU-IDFS-LINE    PIC X(8).                                    
005000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005100        05 REQU-TIAVIDAT-LINE                                             
005200                             PIC X(6).                                    
005300*                                 AVISERINGSDATUM (YYMMDD)                
005400*** END OF VILMAII-COPY LENGTH= 11099 BYTES                               
