000100 01  BIPA-W411BIPA.                                                       
000200*                                 LÄNKAREA TILL W411BIPA -                
000300*                                 KONTROLL OM BIPACKNING SKALL            
000400*                                                                         
000500*                                 OM UTSKRIFT PÅGÅR SKALL IDPRC           
000600*                                 VARA IFYLLD                             
000700*                                                                         
000800*                                 BIPA-KVBIPACK INNEHÅLLER                
000900*                                 ANTALET RADER SOM MAX KAN               
001000*                                 BIPACKAS                                
001100     03 BIPA-INDATA.                                                      
001200        05 BIPA-KDORDBEH     PIC S9              COMP-3.                  
001300*                                 STATUSKOD ORDERBEHANDLING               
001400        05 BIPA-IDDISTR      PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600        05 BIPA-IDKUNDNR     PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800        05 BIPA-IDKUNDRF     PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000        05 BIPA-KDTPOTYP     PIC S9              COMP-3.                  
002100*                                 TYP AV TIDPLANERAD ORDER                
002200        05 BIPA-KDORDKL      PIC S9              COMP-3.                  
002300*                                 ORDERKLASS                              
002400        05 BIPA-KDFAKTYP     PIC X.                                       
002500*                                 FAKTURATYP                              
002600        05 BIPA-IDSYSTEM     PIC X(4).                                    
002700*                                 VOLVO VCCS SYSTEMNUMMER                 
002800        05 BIPA-IDKAMPRF     PIC S9(7)           COMP-3.                  
002900*                                 KAMPANJREFERENS                         
003000        05 BIPA-IDKONTO      PIC S9(11)          COMP-3.                  
003100*                                 KONTO                                   
003200        05 BIPA-IDANALYS     PIC X(12).                                   
003300*                                 ANALYSNUMMER                            
003400        05 BIPA-IDDC         PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600        05 BIPA-IDKST        PIC X(10).                                   
003700*                                 KOSTNADSSTÄLLE                          
003800        05 BIPA-IDPRC-RAD.                                                
003900*                                 PRODUKTIONSKANAL                        
004000           07 BIPA-IDPRCBAS  PIC X(3).                                    
004100*                                 PRC-BAS                                 
004200           07 BIPA-IDPRCVAR  PIC X.                                       
004300*                                 PRC-VARIANT                             
004400        05 BIPA-KVBIPACK     PIC S9(5)           COMP-3.                  
004500*                                 ANTAL BIPACKADE RADER                   
004600        05 BIPA-FLFORBI      PIC X.                                       
004700*                                 FÖRBIORDERFLAGGA                        
004800        05 BIPA-FLORDSPE     PIC X.                                       
004900*                                 SPECIALORDERFLAGGA                      
005000        05 BIPA-FLOVRLEV     PIC X.                                       
005100*                                 ÖVERLEVERANS                            
005200        05 BIPA-BEVARREF     PIC X(10).                                   
005300*                                 VÅR REFERENS                            
005400        05 BIPA-IDBIPREF     PIC X(7).                                    
005500*                                 BIPACKNINGSREFERENS                     
005600        05 BIPA-KDROPACK     PIC X.                                       
005700*                                 FRISLÄPPNINGSKOD RO/DO                  
005800        05 BIPA-KDFRAKT      PIC S9(3)           COMP-3.                  
005900*                                 FRAKTSÄTT DC TILL KUND                  
006000        05 BIPA-IDPRC-LAGOMR OCCURS 99 TIMES.                             
006100*                                 PRODUKTIONSKANAL                        
006200           07 BIPA-IDPRCBAS  PIC X(3).                                    
006300*                                 PRC-BAS                                 
006400           07 BIPA-IDPRCVAR  PIC X.                                       
006500*                                 PRC-VARIANT                             
006600     03 BIPA-UTDATA          OCCURS 13 TIMES.                             
006700        05 BIPA-IDKUNDRF-UT  PIC X(10).                                   
006800*                                 KUNDENS REFERENS (ORDERID)              
006900        05 BIPA-IDARTNR      PIC S9(9)           COMP-3.                  
007000*                                 ARTIKELNUMMER                           
007100        05 BIPA-IDLOPNR      PIC S9(3)           COMP-3.                  
007200*                                 LÖPNUMMER                               
007300        05 BIPA-BERADREF     PIC X(10).                                   
007400*                                 KUNDENS RADREFERENS                     
007500        05 BIPA-IDKONTO-UT   PIC S9(11)          COMP-3.                  
007600*                                 KONTO                                   
007700        05 BIPA-IDKST-UT     PIC X(10).                                   
007800*                                 KOSTNADSSTÄLLE                          
007900        05 BIPA-IDANALYS-UT  PIC X(12).                                   
008000*                                 ANALYSNUMMER                            
008100        05 BIPA-IDSYSTEM-UT  PIC X(4).                                    
008200*                                 VOLVO VCCS SYSTEMNUMMER                 
008300        05 BIPA-KDDSP        PIC S9              COMP-3.                  
008400*                                 PÅVERKAN PÅ DSP                         
008500        05 BIPA-KDKVBRYT     PIC S9              COMP-3.                  
008600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008700        05 BIPA-KDORDING     PIC S9              COMP-3.                  
008800*                                 UPPDATERING ORDERINGÅNG                 
008900        05 BIPA-KDORDKL-UT   PIC S9              COMP-3.                  
009000*                                 ORDERKLASS                              
009100        05 BIPA-KDOI         PIC X(2).                                    
009200*                                 ORDERINGÅNGSTYP                         
009300        05 BIPA-CLEARGROUP.                                               
009400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
009500           07 BIPA-CLEARAREA OCCURS 7 TIMES.                              
009600*                                 CLEARINGAREA FÖR ORDERINGÅNG            
009700              09 BIPA-IDDC-CLEAR                                          
009800                             PIC X(2).                                    
009900*                                 LAGERPRIORITERING VID                   
010000*                                 ORDERCLEARING                           
010100              09 BIPA-FLLF   PIC X.                                       
010200*                                 ARTIKEL LAGERFÖRES                      
010300              09 BIPA-FLCLEAR                                             
010400                             PIC X.                                       
010500*                                 ORDERRAD CLEAR FLAGGA                   
010600        05 BIPA-KDPRODSL     PIC S9(3)           COMP-3.                  
010700*                                 PRODUKTSLAG                             
010800        05 BIPA-KDVRINFO     PIC S9              COMP-3.                  
010900*                                 PÅVERKAN I VR/DSP SYSTEM                
011000        05 BIPA-KDTPOTYP-UT  PIC S9              COMP-3.                  
011100*                                 TYP AV TIDPLANERAD ORDER                
011200        05 BIPA-KDFRAKT-UT   PIC S9(3)           COMP-3.                  
011300*                                 FRAKTSÄTT DC TILL KUND                  
011400        05 BIPA-KVART        PIC S9(7)           COMP-3.                  
011500*                                 ANTAL ARTNR PER BRYTBEGREPP             
011600        05 BIPA-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
011700*                                 ARTIKELPRIS NETTO                       
011800        05 BIPA-PRAVCOST     PIC S9(7)V9(2)      COMP-3.                  
011900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
012000        05 BIPA-FLPRTILL     PIC X.                                       
012100*                                 PRISTILLÄGGS FLAGGA                     
012200        05 BIPA-KDPRTYP      PIC X.                                       
012300*                                 TYP AV PRISTILLÄMPNING                  
012400        05 BIPA-REKSIFFR     PIC S9              COMP-3.                  
012500*                                 KONTROLLSIFFRA                          
012600        05 BIPA-TIRODAT      PIC S9(7)           COMP-3.                  
012700*                                 RESTORDERDATUM         (ÅÅMMDD)         
012800        05 BIPA-FLINVEST     PIC X.                                       
012900*                                 BYTES INVENTERINGSFLAGGA                
013000        05 BIPA-BEVOLREF     PIC X(10).                                   
013100*                                 VOLVO REFERENS                          
013200        05 BIPA-IDLEVNR      PIC X(5).                                    
013300*                                 LEVERANTÖRNUMMER                        
013400        05 BIPA-IDDC-UT      PIC X(2).                                    
013500*                                 IDENTIFIERARE LAGER                     
013600        05 BIPA-IDDC-RO      PIC X(2).                                    
013700*                                 LAGER DÄR RESTORDER FÅR SKE             
013800        05 BIPA-TITPO        PIC S9(7)           COMP-3.                  
013900*                                 PLANERAD ORDERDATUM                     
014000        05 BIPA-FLERS        PIC X.                                       
014100*                                 TILLKOMMANDE ARTIKEL ?                  
014200        05 BIPA-IDKAMPRF-UT  PIC S9(7)           COMP-3.                  
014300*                                 KAMPANJREFERENS                         
014400        05 BIPA-KVBEART-Q    PIC S9(7)           COMP-3.                  
014500*                                 BESTÄLLT KVANTANPASSAT ANTAL            
014600        05 BIPA-TIREGDAT     PIC S9(7)           COMP-3.                  
014700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
014800        05 BIPA-KDORDTYP-LDC PIC X(2).                                    
014900*                                 ORDERTYP HOS DEALER                     
015000        05 BIPA-TIREPDAT     PIC S9(7)           COMP-3.                  
015100*                                 REPAIR DATE                             
015200        05 BIPA-IDKUNDRF-WIP PIC X(10).                                   
015300*                                 REPARATIONS ORDERNR, LDC KUND           
015400        05 BIPA-DEAL-PR-LINE.                                             
015500*                                 DEALERPRIS (RAD)                        
015600           07 BIPA-IDPRQUES  PIC 9(7).                                    
015700*                                 PRISFRÅGA NR                            
015800           07 BIPA-PRARTNTO-LOC                                           
015900                             PIC S9(7)V9(2)      COMP-3.                  
016000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
016100           07 BIPA-PRARTNTO-LOCPREL                                       
016200                             PIC S9(7)V9(2)      COMP-3.                  
016300*                                 PREL NETTO SLUTKUNDSPRIS I              
016400*                                 LOKAL VALUTA                            
016500           07 BIPA-PRARTBTO-LOC                                           
016600                             PIC S9(7)V9(2)      COMP-3.                  
016700*                                 PRIS I LOKAL VALUTA                     
016800           07 BIPA-KDVALISO  PIC X(3).                                    
016900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
017000           07 BIPA-KDVAT     PIC X(2).                                    
017100*                                 MOMSKOD                                 
017200           07 BIPA-RERAB     PIC S9(2)V9(1)      COMP-3.                  
017300*                                 RABATTSATS (PROCENT)                    
017400           07 BIPA-KDRAB     PIC X(5).                                    
017500*                                 RABATTKOD                               
017600           07 BIPA-BEART-VIPS                                             
017700                             PIC X(25).                                   
017800*                                 VIPS ARTIKELBENÄMNING                   
017900*                                 PÅ DEALERNS SPRÅK                       
018000*** END OF VILMAII-COPY LENGTH= 3501 BYTES                                
