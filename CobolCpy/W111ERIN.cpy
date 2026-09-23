000100 01  REQU-W111ERIN.                                                       
000200     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 REQU-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 REQU-IDKORTNR-SPAR1  PIC X(3).                                    
000700*                                 KORTNUMMER                              
000800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000900     03 REQU-IDKORTNR-SPAR2  PIC X(3).                                    
001000*                                 KORTNUMMER                              
001100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001200     03 REQU-IDKORTNR-SPAR3  PIC X(3).                                    
001300*                                 KORTNUMMER                              
001400*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001500     03 REQU-DIERS-ERS       PIC X(7).                                    
001600*                                 KVANTITET I ERSÄTTN.                    
001700     03 REQU-KDERS           PIC X(2).                                    
001800*                                 ERSÄTTNINGSKOD                          
001900     03 REQU-IDAO            PIC X(10).                                   
002000*                                 ÄNDRINGSORDERNUMMER                     
002100     03 REQU-TIERSDAT-PREL   PIC X(5).                                    
002200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
002300     03 REQU-TEARTNOT        PIC X(40).                                   
002400*                                 ARTIKEL NOTERING                        
002500     03 REQU-FLKLAR          PIC X.                                       
002600*                                 AVSLUTNINGSMARKERING                    
002700     03 REQU-IDSPRAK         PIC X(2).                                    
002800*                                 2-STÄLLIG ISO SPRÅKKOD                  
002900     03 REQU-KDARBTYP-SEC-IDLEV                                           
003000                             PIC X(8).                                    
003100*                                 ARBETSTYP FÖR SÄKERHET PÅ LEV           
003200     03 REQU-KDARTSYS        PIC X(2).                                    
003300*                                 KOD FÖR SYST. ÄGARE AV ARTIKEL          
003400     03 REQU-KVRADER-MAX9    PIC 9(5).                                    
003500*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003600*                                 EDAN.                                   
003700     03 REQU-RAD             OCCURS 1 TO 99 TIMES                         
003800                             DEPENDING ON REQU-KVRADER-MAX9.              
003900        05 REQU-IDKORTNR     PIC X(3).                                    
004000*                                 KORTNUMMER                              
004100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004200        05 REQU-FLTEXT       PIC X.                                       
004300*                                 FINNS TEXTINFORMATION ?                 
004400        05 REQU-IDARTNR-TILLK                                             
004500                             PIC X(9).                                    
004600*                                 ARTIKELNUMMER                           
004700        05 REQU-DIERS-TILLK  PIC X(7).                                    
004800*                                 KVANTITET I ERSÄTTN.                    
004900        05 REQU-BEERS        PIC X(20).                                   
005000*                                 ERSÄTTNINGSTEXT                         
005100*** END OF VILMAII-COPY LENGTH= 4062 BYTES                                
