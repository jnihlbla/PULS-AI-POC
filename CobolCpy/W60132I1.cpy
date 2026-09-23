000100 01  REQU-W60132I1.                                                       
000200*                                 COPYTEXT FÖR REQU TILL W6013210         
000300     03 REQU-ADINLOMR-KEY    PIC X(4).                                    
000400*                                 INLEVERANSOMRÅDE                        
000500     03 REQU-ADINLOMR-NXT-KEY                                             
000600                             PIC X(4).                                    
000700*                                 INLEVERANSOMRÅDE NÄSTA                  
000800     03 REQU-KDINLQ-KEY      PIC X.                                       
000900*                                 INLEVERANSKÖTYP KOLLI/PARTI             
001000     03 REQU-BEFT-FOM-KEY    PIC X(2).                                    
001100*                                 FÖRPACKNINGSTYP                         
001200     03 REQU-BEFT-TOM-KEY    PIC X(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 REQU-FLINLFB-KEY     PIC X.                                       
001500*                                 VALD TILL FÖRBEHANDLING                 
001600     03 REQU-IDDC-KEY        PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 REQU-IDLEVNR-START   PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 REQU-IDFS-START      PIC X(8).                                    
002100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002200     03 REQU-TIAVIDAT-START  PIC 9(6).                                    
002300*                                 AVISERINGSDATUM (YYMMDD)                
002400     03 REQU-IDRADNR-INL-START                                            
002500                             PIC 9(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 REQU-IDRADNR-START   PIC 9(5).                                    
002800*                                 RADNUMMER                               
002900     03 REQU-KDINLPRIO-START PIC 9(2).                                    
003000*                                 PRIORITETSGRUPP                         
003100     03 REQU-IDINLVGN-SPAR   OCCURS 51 TIMES                              
003200                             PIC 9(3).                                    
003300*                                 VAGNSIDENTITET                          
003400     03 REQU-KVINLCAR        PIC X(5).                                    
003500*                                 ANTAL VAGNAR PÅ KÖ                      
003600     03 REQU-KVRADER-HIT     PIC X(5).                                    
003700*                                 ANTAL VISADE RADER HITTILS              
003800     03 REQU-KVRADER-TOT     PIC X(5).                                    
003900*                                 TOTALT ANTAL RADER                      
004000     03 REQU-KVRADER-PRIO    PIC X(5).                                    
004100*                                 ANTAL PRIORITERADE RADER                
004200     03 REQU-KVRADER         PIC 9(5).                                    
004300*                                 ANTAL RADER                             
004400     03 REQU-INPUT           OCCURS 1000 TIMES.                           
004500        05 REQU-KDCMDVAL-LINE                                             
004600                             PIC X(3).                                    
004700*                                 GENERELL KOMMANDOKOD                    
004800        05 REQU-IDLOPNRM-LINE                                             
004900                             PIC 9(9).                                    
005000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005100*                                 (0VVDLLLLK)                             
005200        05 REQU-IDRADNR-LINE PIC 9(3).                                    
005300*                                 RADNUMMER                               
005400*** END OF VILMAII-COPY LENGTH= 15229 BYTES                               
