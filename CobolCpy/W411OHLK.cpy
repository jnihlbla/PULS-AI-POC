000100 01  OHLK-W411OHLK.                                                       
000200*                                 LÄNKAREA TILL W411OHLK - LOGISK         
000300*                                  KONTROLL AV ORDERHUVUDET               
000400     03 OHLK-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 OHLK-IDANALYS        PIC X(12).                                   
000700*                                 ANALYSNUMMER                            
000800     03 OHLK-IDANALYS-OK     PIC X.                                       
000900     03 OHLK-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 OHLK-IDDISTR-OK      PIC X.                                       
001200     03 OHLK-IDKUNDNR        PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 OHLK-IDORDNR         PIC 9(7).                                    
001500*                                 ORDERNUMMER                             
001600     03 OHLK-IDORDNR-OK      PIC X.                                       
001700     03 OHLK-KDORDKL         PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900     03 OHLK-KDORDKL-OK      PIC X.                                       
002000     03 OHLK-SEC-KDSVAR      PIC X.                                       
002100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002200     03 OHLK-FLAUTORD        PIC X.                                       
002300*                                 FLAGGAN STYR OM AUTOMATORDER-           
002400*                                 NUMMER SKALL SKAPAS                     
002500     03 OHLK-FLOKFAK-G       PIC X.                                       
002600*                                 FLAGGA FAKTURATYP G GODKÄND             
002700     03 OHLK-FLOKFAK-N       PIC X.                                       
002800*                                 FLAGGA FAKTURATYP N GODKÄND             
002900     03 OHLK-FLOKFAK-R       PIC X.                                       
003000*                                 FLAGGA FAKTURATYP R GODKÄND             
003100     03 OHLK-FLOKFAK-K       PIC X.                                       
003200*                                 FLAGGA FAKTURATYP K GODKÄND             
003300     03 OHLK-FLORDSPE        PIC X.                                       
003400*                                 SPECIALORDERFLAGGA                      
003500     03 OHLK-FLVORKO         PIC X.                                       
003600*                                 VOR-KÖ FLAGGA                           
003700     03 OHLK-IDFTG           PIC 9(2).                                    
003800*                                 FÖRETAGSID EKONOM REDOVISNING           
003900     03 OHLK-IDFTG-OK        PIC X.                                       
004000     03 OHLK-IDKONTO         PIC S9(11)          COMP-3.                  
004100*                                 KONTO                                   
004200     03 OHLK-IDKONTO-OK      PIC X.                                       
004300     03 OHLK-IDKST           PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500     03 OHLK-IDKST-OK        PIC X.                                       
004600     03 OHLK-IDKAMPRF        PIC S9(7)           COMP-3.                  
004700*                                 KAMPANJREFERENS                         
004800     03 OHLK-IDKAMPRF-OK     PIC X.                                       
004900     03 OHLK-IDDC            PIC X(2).                                    
005000*                                 IDENTIFIERARE LAGER                     
005100     03 OHLK-IDDC-TVS        PIC X(2).                                    
005200*                                 DISTRIBUTIONCENTER                      
005300*                                 TVÅNGSSTYRNING                          
005400     03 OHLK-IDDC-TVS-OK     PIC X.                                       
005500     03 OHLK-KDFAKTYP        PIC X.                                       
005600*                                 FAKTURATYP                              
005700     03 OHLK-KDFAKTYP-OK     PIC X.                                       
005800     03 OHLK-KDTPOTYP        PIC S9              COMP-3.                  
005900*                                 TYP AV TIDPLANERAD ORDER                
006000     03 OHLK-KDTPOTYP-OK     PIC X.                                       
006100     03 OHLK-TITPO           PIC S9(7)           COMP-3.                  
006200*                                 PLANERAD ORDERDATUM                     
006300     03 OHLK-TITPO-OK        PIC X.                                       
006400*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
