000100 01  201-W414201A.                                                        
000200*                                 201                                     
000300*                                 SKAPAS FÖR ORDERRAD VID                 
000400*                                 ORDERENTRY.                             
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 201-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 201-BERADREF         PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100     03 201-BEVARREF         PIC X(10).                                   
001200*                                 VÅR REFERENS                            
001300     03 201-BEVOLREF         PIC X(10).                                   
001400*                                 VOLVO REFERENS                          
001500     03 201-FLINVEST         PIC X.                                       
001600*                                 BYTES INVENTERINGSFLAGGA                
001700     03 201-FLOVRLEV         PIC X.                                       
001800*                                 ÖVERLEVERANS                            
001900     03 201-FLPRTILL         PIC X.                                       
002000*                                 PRISTILLÄGGS FLAGGA                     
002100     03 201-FLTILLK          PIC X.                                       
002200*                                 TILLKOMMANDE ARTIKEL ?                  
002300     03 201-FLRADTVS         PIC X.                                       
002400*                                 ORDERRAD TVÅNGSSTYRD GENOM TABE         
002500*                                 LL ?                                    
002600     03 201-IDARBREF         PIC X(10).                                   
002700*                                 ARBETSORDER VADIS                       
002800     03 201-IDDISTR          PIC S9(5)           COMP-3.                  
002900*                                 DISTRIKTNUMMER                          
003000     03 201-IDBIL.                                                        
003100*                                 BILIDENTITET                            
003200        05 201-IDBILTYP      PIC X(3).                                    
003300*                                 BILTYP                                  
003400        05 201-TIAAAA        PIC X(4).                                    
003500*                                 ÅRTAL (ÅÅÅÅ)                            
003600        05 201-IDCHASSI-PIE  PIC X(6).                                    
003700*                                 CHASSINUMMER PIE                        
003800     03 201-IDKAMPRF         PIC S9(7)           COMP-3.                  
003900*                                 KAMPANJREFERENS                         
004000     03 201-IDKLIENT         PIC X(10).                                   
004100*                                 VADIS KLIENT                            
004200     03 201-IDKONTO          PIC S9(11)          COMP-3.                  
004300*                                 KONTO                                   
004400     03 201-IDKST            PIC X(10).                                   
004500*                                 KOSTNADSSTÄLLE                          
004600     03 201-IDKUNDNR         PIC S9(7)           COMP-3.                  
004700*                                 KUNDNUMMER                              
004800     03 201-IDKUNDRF         PIC X(10).                                   
004900*                                 KUNDENS REFERENS (ORDERID)              
005000     03 201-IDKUNDRF-RO      PIC X(10).                                   
005100*                                 KUND REF PÅ RO                          
005200     03 201-IDLOPNR          PIC S9(3)           COMP-3.                  
005300*                                 LÖPNUMMER                               
005400     03 201-IDORDER          PIC S9(7)           COMP-3.                  
005500*                                 VOLVO PARTS ORDERNUMMER                 
005600     03 201-IDSYSTEM         PIC X(4).                                    
005700*                                 VOLVO VCCS SYSTEMNUMMER                 
005800     03 201-IDSYSTEM-OHUV    PIC X(4).                                    
005900*                                 VOLVO VCCS SYSTEMNUMMER                 
006000     03 201-IDUSER           PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200     03 201-IDVIN            PIC X(17).                                   
006300*                                 VIN ID FORDON                           
006400     03 201-IDDC             PIC X(2).                                    
006500*                                 IDENTIFIERARE LAGER                     
006600     03 201-IDDC-CLEAR       PIC X(2).                                    
006700*                                 LAGERPRIORITERING VID                   
006800*                                 ORDERCLEARING                           
006900     03 201-KDDSP            PIC S9              COMP-3.                  
007000*                                 PÅVERKAN PÅ DSP                         
007100     03 201-KDFAKTYP         PIC X.                                       
007200*                                 FAKTURATYP                              
007300     03 201-KDFRAKT          PIC S9(3)           COMP-3.                  
007400*                                 FRAKTSÄTT DC TILL KUND                  
007500     03 201-KDKVBRYT         PIC S9              COMP-3.                  
007600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007700     03 201-KDOI             PIC X(2).                                    
007800*                                 ORDERINGÅNGSTYP                         
007900     03 201-KDORDING         PIC S9              COMP-3.                  
008000*                                 UPPDATERING ORDERINGÅNG                 
008100     03 201-KDORDKL          PIC S9              COMP-3.                  
008200*                                 ORDERKLASS                              
008300     03 201-KDPRTYP          PIC X.                                       
008400*                                 TYP AV PRISTILLÄMPNING                  
008500     03 201-KDTPOTYP         PIC S9              COMP-3.                  
008600*                                 TYP AV TIDPLANERAD ORDER                
008700     03 201-KDVRINFO         PIC S9              COMP-3.                  
008800*                                 PÅVERKAN I VR/DSP SYSTEM                
008900     03 201-KVBEART          PIC S9(7)           COMP-3.                  
009000*                                 BESTÄLLT ANTAL STYCKEN                  
009100     03 201-KVBEART-Q        PIC S9(7)           COMP-3.                  
009200*                                 BESTÄLLT KVANTANPASSAT ANTAL            
009300     03 201-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
009400*                                 ARTIKELPRIS NETTO                       
009500     03 201-PRBPRIS          PIC S9(7)V9(2)      COMP-3.                  
009600*                                 BASPRIS                                 
009700     03 201-REKSIFFR         PIC S9              COMP-3.                  
009800*                                 KONTROLLSIFFRA                          
009900     03 201-TIREGDAT         PIC S9(7)           COMP-3.                  
010000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010100     03 201-TIRODAT          PIC S9(7)           COMP-3.                  
010200*                                 RESTORDERDATUM         (ÅÅMMDD)         
010300*** END OF VILMAII-COPY LENGTH= 202 BYTES                                 
