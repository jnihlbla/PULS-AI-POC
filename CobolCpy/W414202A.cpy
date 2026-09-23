000100 01  202-W414202A.                                                        
000200*                                 202                                     
000300*                                 SKAPAS FÖR ORDERBEKRÄFTELSE-            
000400*                                 RAD VID ORDERENTRY.                     
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 202-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 202-BEERS            PIC X(20).                                   
001000*                                 ERSÄTTNINGSTEXT                         
001100     03 202-BERADREF         PIC X(10).                                   
001200*                                 KUNDENS RADREFERENS                     
001300     03 202-BEVARREF         PIC X(10).                                   
001400*                                 VÅR REFERENS                            
001500     03 202-BEVOLREF         PIC X(10).                                   
001600*                                 VOLVO REFERENS                          
001700     03 202-DIERS-KVOT       PIC S9(4)V9(3)      COMP-3.                  
001800*                                 KVOT MELLAN                             
001900*                                 DIERS-TILLK OCH DIERS-ERS               
002000     03 202-FLINVEST         PIC X.                                       
002100*                                 BYTES INVENTERINGSFLAGGA                
002200     03 202-FLOVRLEV         PIC X.                                       
002300*                                 ÖVERLEVERANS                            
002400     03 202-FLPRTILL         PIC X.                                       
002500*                                 PRISTILLÄGGS FLAGGA                     
002600     03 202-FLTILLK          PIC X.                                       
002700*                                 TILLKOMMANDE ARTIKEL ?                  
002800     03 202-IDARTNR-TILLK    PIC S9(9)           COMP-3.                  
002900*                                 TILLKOMMANDE ARTIKELNUMMER              
003000     03 202-IDDISTR          PIC S9(5)           COMP-3.                  
003100*                                 DISTRIKTNUMMER                          
003200     03 202-IDBIL.                                                        
003300*                                 BILIDENTITET                            
003400        05 202-IDBILTYP      PIC X(3).                                    
003500*                                 BILTYP                                  
003600        05 202-TIAAAA        PIC X(4).                                    
003700*                                 ÅRTAL (ÅÅÅÅ)                            
003800        05 202-IDCHASSI-PIE  PIC X(6).                                    
003900*                                 CHASSINUMMER PIE                        
004000     03 202-IDKONTO          PIC S9(11)          COMP-3.                  
004100*                                 KONTO                                   
004200     03 202-IDKST            PIC X(10).                                   
004300*                                 KOSTNADSSTÄLLE                          
004400     03 202-IDKUNDNR         PIC S9(7)           COMP-3.                  
004500*                                 KUNDNUMMER                              
004600     03 202-IDKUNDRF         PIC X(10).                                   
004700*                                 KUNDENS REFERENS (ORDERID)              
004800     03 202-IDKUNDRF-RO      PIC X(10).                                   
004900*                                 KUND REF PÅ RO                          
005000     03 202-IDLOPNR          PIC S9(3)           COMP-3.                  
005100*                                 LÖPNUMMER                               
005200     03 202-IDORDER          PIC S9(7)           COMP-3.                  
005300*                                 VOLVO PARTS ORDERNUMMER                 
005400     03 202-IDSEKVNR         PIC S9(3)           COMP-3.                  
005500*                                 GENERELLT SEKVENSNUMMER                 
005600     03 202-IDSYSTEM         PIC X(4).                                    
005700*                                 VOLVO VCCS SYSTEMNUMMER                 
005800     03 202-IDDC             PIC X(2).                                    
005900*                                 IDENTIFIERARE LAGER                     
006000     03 202-KDDSP            PIC S9              COMP-3.                  
006100*                                 PÅVERKAN PÅ DSP                         
006200     03 202-KDERS            PIC S9(3)           COMP-3.                  
006300*                                 ERSÄTTNINGSKOD                          
006400     03 202-KDFAKTYP         PIC X.                                       
006500*                                 FAKTURATYP                              
006600     03 202-KDFRAKT          PIC S9(3)           COMP-3.                  
006700*                                 FRAKTSÄTT DC TILL KUND                  
006800     03 202-KDKVBRYT         PIC S9              COMP-3.                  
006900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007000     03 202-KDORDBEK         PIC 9(2).                                    
007100*                                 ORDERBEKRÄFTELSEKOD                     
007200     03 202-KDORDKL          PIC S9              COMP-3.                  
007300*                                 ORDERKLASS                              
007400     03 202-KDPRTYP          PIC X.                                       
007500*                                 TYP AV PRISTILLÄMPNING                  
007600     03 202-KDTPOTYP         PIC S9              COMP-3.                  
007700*                                 TYP AV TIDPLANERAD ORDER                
007800     03 202-KDVRINFO         PIC S9              COMP-3.                  
007900*                                 PÅVERKAN I VR/DSP SYSTEM                
008000     03 202-KVBEART          PIC S9(7)           COMP-3.                  
008100*                                 BESTÄLLT ANTAL STYCKEN                  
008200     03 202-KVBEART-Q        PIC S9(7)           COMP-3.                  
008300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
008400     03 202-KVBEART-TILLK    PIC S9(7)           COMP-3.                  
008500*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
008600     03 202-KVPREAVB         PIC S9(7)           COMP-3.                  
008700*                                 PREL-AVB KVANT                          
008800     03 202-KVPRERO          PIC S9(7)           COMP-3.                  
008900*                                 PRELIMINÄR RO-KVANT                     
009000     03 202-KVQPACK-1        PIC S9(5)           COMP-3.                  
009100*                                 ANTAL I Q1 FÖRPACKNING                  
009200     03 202-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
009300*                                 ARTIKELPRIS NETTO                       
009400     03 202-REKSIFFR         PIC S9              COMP-3.                  
009500*                                 KONTROLLSIFFRA                          
009600     03 202-REKSIFFR-TILLK   PIC S9              COMP-3.                  
009700*                                 TILLKOMMANDE KONTROLLSIFFRA             
009800     03 202-TIDISPIN         PIC S9(7)           COMP-3.                  
009900*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
010000     03 202-TIORDREG         PIC S9(7)           COMP-3.                  
010100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
010200     03 202-TIREGDAT         PIC S9(7)           COMP-3.                  
010300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010400     03 202-TITPO            PIC S9(7)           COMP-3.                  
010500*                                 PLANERAD ORDERDATUM                     
010600     03 202-TIREPDAT         PIC S9(7)           COMP-3.                  
010700*                                 REPAIR DATE                             
010800*** END OF VILMAII-COPY LENGTH= 201 BYTES                                 
