000100 01  OHUV-W412500.                                                        
000200*                                 ORDERHUVUD TRANSAKTIONER VR,ÖVR         
000300*                                 POSTTYP = R50, RG0                      
000400     03 OHUV-TIFILDAT        PIC X(6).                                    
000500*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000600     03 OHUV-TIKLOCK         PIC X(8).                                    
000700*                                 KLOCKSLAG (TTMMSSTH)                    
000800     03 OHUV-IDCPYTXT.                                                    
000900*                                 COPYTEXT IDENTITET                      
001000        05 OHUV-CT-IDSYSTEM  PIC X(4).                                    
001100*                                 VOLVO VCCS SYSTEMNUMMER                 
001200        05 OHUV-CT-IDPTYP    PIC X(3).                                    
001300*                                 POSTTYP                                 
001400        05 OHUV-CT-IDVTYP    PIC X.                                       
001500*                                 POSTTYPSVERSION                         
001600     03 OHUV-IDPTYP          PIC X(3).                                    
001700*                                 POSTTYP                                 
001800     03 OHUV-IDSYSTEM        PIC X(4).                                    
001900*                                 VOLVO VCCS SYSTEMNUMMER                 
002000     03 OHUV-IDDISTR         PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 OHUV-IDKUNDNR        PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400     03 OHUV-IDORDNR         PIC X(7).                                    
002500*                                 ORDERNUMMER                             
002600     03 OHUV-KDORDKL         PIC X.                                       
002700*                                 ORDERKLASS                              
002800     03 OHUV-KDFRAKT         PIC X(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000     03 OHUV-TIRFS           PIC X(6).                                    
003100     03 OHUV-BEKUNDRF        PIC X(15).                                   
003200*                                 KUNDENS REFERENS                        
003300     03 OHUV-TIKUNDRF        PIC X(6).                                    
003400*                                 REFERENSDATUM (ÅÅMMDD)                  
003500     03 OHUV-KDFAKTYP        PIC X.                                       
003600*                                 FAKTURATYP                              
003700     03 OHUV-FLRESTN         PIC X.                                       
003800*                                 RESTNOTERING ?                          
003900     03 OHUV-KDTPOTYP        PIC X.                                       
004000*                                 TYP AV TIDPLANERAD ORDER                
004100     03 OHUV-TITPO           PIC X(6).                                    
004200*                                 PLANERAD ORDERDATUM                     
004300     03 OHUV-BELAGINS        PIC X(60).                                   
004400*                                 DEL AV LAGERINSTRUKTION                 
004500     03 OHUV-BEGODSM.                                                     
004600*                                 GODSMOTTAGARNAMN                        
004700        05 OHUV-BEGODSM-RAD1 PIC X(27).                                   
004800*                                 GODSMOTTAGARNAMN RAD 1                  
004900        05 OHUV-BEGODSM-RAD2 PIC X(27).                                   
005000*                                 GODSMOTTAGARNAMN RAD 2                  
005100     03 OHUV-ADGODSM.                                                     
005200*                                 GODSMOTTAGARADRESS                      
005300        05 OHUV-ADGODSM-RAD1 PIC X(27).                                   
005400*                                 GODSMOTTAGARADRESS RAD 1                
005500        05 OHUV-ADGODSM-RAD2 PIC X(27).                                   
005600*                                 GODSMOTTAGARADRESS RAD 2                
005700     03 OHUV-KDROPACK        PIC X.                                       
005800*                                 FRISLÄPPNINGSKOD RO/DO                  
005900     03 OHUV-IDKONTO         PIC X(10).                                   
006000*                                 KONTO                                   
006100     03 OHUV-IDKST           PIC X(10).                                   
006200*                                 KOSTNADSSTÄLLE                          
006300     03 OHUV-IDANALYS        PIC X(12).                                   
006400*                                 ANALYSNUMMER                            
006500     03 OHUV-BEVARREF        PIC X(10).                                   
006600*                                 VÅR REFERENS                            
006700     03 OHUV-KDTULLVE        PIC X.                                       
006800*                                 TYP AV PRIS PÅ TULLFAKTURA              
006900     03 OHUV-KDNOTES         PIC X(2).                                    
007000*                                 NOTERINGSKOD                            
007100     03 OHUV-FLAUTFAK        PIC X.                                       
007200*                                 AUTOMATFAKTURERING ?                    
007300     03 OHUV-FLAUTPAC        PIC X.                                       
007400*                                 AUTOMATISK PACKRAPPORTERING             
007500     03 OHUV-FLEMBORD        PIC X.                                       
007600*                                 EMBALLAGEORDER ?                        
007700     03 OHUV-FLOVRLEV        PIC X.                                       
007800*                                 ÖVERLEVERANS                            
007900     03 OHUV-IDGROSS         PIC X(3).                                    
008000*                                 GROSSIST KUNDNUMMER FRÅN TACDIS         
008100     03 OHUV-IDFTG           PIC X(2).                                    
008200*                                 FÖRETAGSID EKONOM REDOVISNING           
008300     03 OHUV-IDDC            PIC X(2).                                    
008400*                                 IDENTIFIERARE LAGER                     
008500     03 OHUV-FLLSBOK         PIC X.                                       
008600*                                 LAGERAVBOKNING                          
008700     03 OHUV-IDDEPT          PIC 9(2).                                    
008800*                                 AVDELNING I VERKSTAD                    
008900     03 OHUV-KDORDTYP-LDC    PIC X(2).                                    
009000*                                 ORDERTYP HOS DEALER                     
009100     03 OHUV-TIREPDAT        PIC 9(6).                                    
009200*                                 REPAIR DATE                             
009300     03 OHUV-IDBILREG        PIC X(10).                                   
009400*                                 BILENS REGISTRERINGSNUMMER              
009500     03 OHUV-IDCISNR         PIC X(12).                                   
009600*                                 CIS NUMMER                              
009700     03 OHUV-IDVIN           PIC X(17).                                   
009800*                                 VIN ID FORDON                           
009900     03 OHUV-TETACDBO        PIC X(35).                                   
010000*                                 REFERENS BUTIK ORDER TACDIS             
010100     03 OHUV-BETELNR-TACD    PIC X(25).                                   
010200*                                 TELEFONNUMMER SMS BUTIKSORDER           
010300     03 OHUV-BEMEKAN         PIC X(15).                                   
010400*                                 FÖRVALD MEKANIKER/VERKSTAD              
010500     03 OHUV-FLFPLOCK        PIC X.                                       
010600*                                 FÖRLEVERANSINDIKATOR                    
010700     03 OHUV-TIHHMM          PIC 9(4).                                    
010800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
010900*** END OF VILMAII-COPY LENGTH= 440 BYTES                                 
