000100 01  ORAD-W412550.                                                        
000200*                                 ORDERRAD TRANSAKTIONER VR,ÖVRIG         
000300*                                 A                                       
000400*                                 POSTTYP = R55, RG5                      
000500     03 ORAD-TIFILDAT        PIC X(6).                                    
000600*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000700     03 ORAD-TIKLOCK         PIC X(8).                                    
000800*                                 KLOCKSLAG (TTMMSSTH)                    
000900     03 ORAD-IDCPYTXT.                                                    
001000*                                 COPYTEXT IDENTITET                      
001100        05 ORAD-CT-IDSYSTEM  PIC X(4).                                    
001200*                                 VOLVO VCCS SYSTEMNUMMER                 
001300        05 ORAD-CT-IDPTYP    PIC X(3).                                    
001400*                                 POSTTYP                                 
001500        05 ORAD-CT-IDVTYP    PIC X.                                       
001600*                                 POSTTYPSVERSION                         
001700     03 ORAD-IDPTYP          PIC X(3).                                    
001800*                                 POSTTYP                                 
001900     03 ORAD-IDSYSTEM        PIC X(4).                                    
002000*                                 VOLVO VCCS SYSTEMNUMMER                 
002100     03 ORAD-IDDISTR         PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 ORAD-IDKUNDNR        PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500     03 ORAD-IDORDNR         PIC X(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 ORAD-IDARTNR         PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 ORAD-REKSIFFR        PIC X.                                       
003000*                                 KONTROLLSIFFRA                          
003100     03 ORAD-KVBEART         PIC X(6).                                    
003200*                                 BESTÄLLT ANTAL STYCKEN                  
003300     03 ORAD-PRARTNTO        PIC X(10).                                   
003400*                                 ARTIKELPRIS NETTO                       
003500     03 ORAD-TITPO           PIC X(6).                                    
003600*                                 PLANERAD ORDERDATUM                     
003700     03 ORAD-FLRESTN         PIC X.                                       
003800*                                 RESTNOTERING ?                          
003900     03 ORAD-KDKVBRYT        PIC X.                                       
004000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004100     03 ORAD-FLINVEST        PIC X.                                       
004200*                                 BYTES INVENTERINGSFLAGGA                
004300     03 ORAD-KDVRINFO        PIC X.                                       
004400*                                 PÅVERKAN I VR/DSP SYSTEM                
004500     03 ORAD-IDKONTO         PIC X(10).                                   
004600*                                 KONTO                                   
004700     03 ORAD-IDKST           PIC X(10).                                   
004800*                                 KOSTNADSSTÄLLE                          
004900     03 ORAD-BERADREF        PIC X(10).                                   
005000*                                 KUNDENS RADREFERENS                     
005100     03 ORAD-BEVOLREF        PIC X(10).                                   
005200*                                 VOLVO REFERENS                          
005300     03 ORAD-KDDSP           PIC X.                                       
005400*                                 PÅVERKAN PÅ DSP                         
005500     03 ORAD-FLSLATT         PIC X.                                       
005600*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005700*                                 LL BERÄKNAS ELLER EJ                    
005800*                                 OM FLRESTN = J OCH FLSLATT = J,         
005900*                                  DÅ BERÄKNAS KVSLATT                    
006000     03 ORAD-IDKLIENT        PIC X(10).                                   
006100*                                 VADIS KLIENT                            
006200     03 ORAD-IDARBREF        PIC X(10).                                   
006300*                                 ARBETSORDER VADIS                       
006400     03 ORAD-IDBIL.                                                       
006500*                                 BILIDENTITET                            
006600        05 ORAD-IDBILTYP     PIC X(3).                                    
006700*                                 BILTYP                                  
006800        05 ORAD-TIAAAA       PIC X(4).                                    
006900*                                 ÅRTAL (ÅÅÅÅ)                            
007000        05 ORAD-IDCHASSI-PIE PIC X(6).                                    
007100*                                 CHASSINUMMER PIE                        
007200     03 ORAD-IDVIN           PIC X(17).                                   
007300*                                 VIN ID FORDON                           
007400     03 ORAD-IDDEPT          PIC 9(2).                                    
007500*                                 AVDELNING I VERKSTAD                    
007600     03 ORAD-IDPRQUES        PIC X(7).                                    
007700*                                 PRISFRÅGA NR                            
007800     03 ORAD-PRARTNTO-LOC    PIC X(10).                                   
007900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008000     03 ORAD-PRARTNTO-LOCPREL                                             
008100                             PIC X(10).                                   
008200*                                 PREL NETTO SLUTKUNDSPRIS I              
008300*                                 LOKAL VALUTA                            
008400     03 ORAD-PRARTBTO-LOC    PIC X(10).                                   
008500*                                 PRIS I LOKAL VALUTA                     
008600     03 ORAD-KDVALISO        PIC X(3).                                    
008700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008800     03 ORAD-KDVAT           PIC X(2).                                    
008900*                                 MOMSKOD                                 
009000     03 ORAD-RERAB           PIC 9(2)V9(1).                               
009100*                                 RABATTSATS (PROCENT)                    
009200     03 ORAD-KDRAB           PIC X(5).                                    
009300*                                 RABATTKOD                               
009400     03 ORAD-BEART-VIPS      PIC X(25).                                   
009500*                                 VIPS ARTIKELBENÄMNING                   
009600*                                 PÅ DEALERNS SPRÅK                       
009700     03 ORAD-ADLAGOMR-CD     PIC 9(2).                                    
009800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
009900     03 ORAD-ADGANG-CD       PIC 9(2).                                    
010000*                                 GÅNG                                    
010100     03 ORAD-ADPLATS-CD      PIC 9(5).                                    
010200*                                 LAGERPLATSNUMMER                        
010300     03 ORAD-IDKUNDRF-WIP    PIC X(10).                                   
010400*                                 REPARATIONS ORDERNR, LDC KUND           
010500*** END OF VILMAII-COPY LENGTH= 270 BYTES                                 
