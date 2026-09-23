000100 01  PU-W46312.                                                           
000200*                                 DIREKTLEVERANSFIL, PACKUNDERLAG         
000300*                                                                         
000400     03 PU-IDPTYP            PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 PU-IDLEVNR           PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 PU-IDPRODNR          PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000     03 PU-HUVUD.                                                         
001100*                                 001 HUVUDPOST                           
001200        05 PU-IDDISTR        PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400        05 PU-IDKUNDNR       PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600        05 PU-KDFRAKT        PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800        05 PU-IDORDNR        PIC S9(5)           COMP-3.                  
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000        05 PU-KDORDKL        PIC S9              COMP-3.                  
002100*                                 ORDERKLASS                              
002200        05 PU-DAUTSKR        PIC 9(8).                                    
002300*                                 UTSKRIFTDATUM  (ÅÅÅÅMMDD)               
002400        05 PU-BELAGINS       PIC X(35).                                   
002500*                                 LAGERINSTRUKTION   BELAGINS-002         
002600        05 PU-DASKEPPN       PIC 9(8).                                    
002700*                                 SKEPPNINGSDATUM  (ÅÅÅÅMMDD)             
002800        05 PU-DASNDDAT       PIC 9(8).                                    
002900*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
003000        05 PU-IDDC           PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200        05 PU-IDDEPOT        PIC X(2).                                    
003300*                                 TRANSPORT DEPOT                         
003400        05 PU-IDROUTE        PIC X.                                       
003500*                                 TRANSPORT ROUTE                         
003600        05 PU-IDVAT          PIC X(17).                                   
003700*                                 MOMSREGISTRERINGSNUMMER                 
003800        05 PU-IDZON          PIC X(2).                                    
003900*                                 TRANSPORTVÄG (RUTT,ZON)                 
004000        05 PU-KDVIA          PIC X(2).                                    
004100*                                 KOD FöR LEVERANS VIA                    
004200        05 PU-TISNDTID       PIC S9(7)           COMP-3.                  
004300*                                 GENERELL SÄNDNINGSTID                   
004400        05 PU-KOMPL.                                                      
004500*                                 002 KOMPLETTERING HUVUD                 
004600           07 PU-BEGDSMRK    PIC X(60).                                   
004700        05 PU-KOMPL2.                                                     
004800*                                 003 KOMPLETTERING 2 HUVUD               
004900           07 PU-BEGMT.                                                   
005000*                                 GODSMOTTAGARNAMN                        
005100              09 PU-BEGMT-RAD1                                            
005200                             PIC X(35).                                   
005300*                                 GODSMOTTAGARNAMN RAD 1                  
005400              09 PU-BEGMT-RAD2                                            
005500                             PIC X(35).                                   
005600*                                 GODSMOTTAGARNAMN RAD 2                  
005700        05 PU-KOMPL3.                                                     
005800*                                 004 KOMPLETTERING 3 HUVUD               
005900           07 PU-ADGMT.                                                   
006000*                                 GODSMOTTAGARADRESS                      
006100              09 PU-ADGMT-GATA                                            
006200                             PIC X(35).                                   
006300*                                 GODSMOTTAGARADRESS GATA                 
006400              09 PU-ADGMT-PADR                                            
006500                             PIC X(35).                                   
006600*                                 GODSMOTTAGARADRESS POSTADRESS           
006700              09 PU-ADPOST-PNRORT REDEFINES PU-ADGMT-PADR.                
006800*                                 POSTNUMMER + ORT                        
006900                 11 PU-ADPOSTNR                                           
007000                             PIC X(10).                                   
007100*                                 POSTNUMMER I ADRESS                     
007200                 11 PU-ADCITY                                             
007300                             PIC X(25).                                   
007400*                                 BENÄMNING PÅ STAD                       
007500              09 PU-ADPOST-ORTPNR REDEFINES PU-ADGMT-PADR.                
007600*                                 ORT + POSTNUMMER                        
007700                 11 PU-ADCITY                                             
007800                             PIC X(25).                                   
007900*                                 BENÄMNING PÅ STAD                       
008000                 11 PU-ADPOSTNR                                           
008100                             PIC X(10).                                   
008200*                                 POSTNUMMER I ADRESS                     
008300              09 PU-ADGMT-LAND                                            
008400                             PIC X(35).                                   
008500*                                 GODSMOTTAGARADRESS LAND                 
008600     03 PU-RAD-FILLER REDEFINES PU-HUVUD.                                 
008700        05 PU-RAD.                                                        
008800*                                 004 RADPOST                             
008900           07 PU-IDRADNR     PIC S9(5)           COMP-3.                  
009000*                                 RADNUMMER                               
009100           07 PU-ADART.                                                   
009200*                                 ARTIKELADRESS I LAGRET                  
009300              09 PU-ADLAGOMR PIC S9(3)           COMP-3.                  
009400*                                 LAGEROMRÅDE                             
009500              09 PU-ADGANG   PIC S9(3)           COMP-3.                  
009600*                                 GÅNG                                    
009700              09 PU-ADPLATS  PIC S9(5)           COMP-3.                  
009800*                                 LAGERPLATSNUMMER                        
009900           07 PU-BEART       PIC X(25).                                   
010000*                                 ARTIKELBENÄMNING                        
010100           07 PU-BERADREF    PIC X(10).                                   
010200*                                 KUNDENS RADREFERENS                     
010300           07 PU-IDARTNR     PIC S9(9)           COMP-3.                  
010400*                                 ARTIKELNUMMER                           
010500           07 PU-KDARTURS    PIC X(2).                                    
010600*                                 ARTIKELURSPRUNGSKOD                     
010700           07 PU-KVBEART     PIC S9(7)           COMP-3.                  
010800*                                 BESTÄLLT ANTAL STYCKEN                  
010900           07 PU-PRARTNTO    PIC S9(7)V9(2)      COMP-3.                  
011000*                                 ARTIKELPRIS NETTO                       
011100           07 PU-TIREPDAT    PIC S9(7)           COMP-3.                  
011200*                                 REPAIR DATE                             
011300           07 PU-IDBILREG    PIC X(10).                                   
011400*                                 BILENS REGISTRERINGSNUMMER              
011500           07 PU-BEMEKAN     PIC X(15).                                   
011600*                                 FÖRVALD MEKANIKER/VERKSTAD              
011700           07 PU-IDKUNDRF-WIP                                             
011800                             PIC X(10).                                   
011900*                                 REPARATIONS ORDERNR, LDC KUND           
012000        05 FILLER            PIC X(237).                                  
012100*** END OF VILMAII-COPY LENGTH= 349 BYTES                                 
