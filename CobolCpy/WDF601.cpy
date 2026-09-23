000100 01  PUDH-WDF601.                                                         
000200*                                 PU-DIREKTLEVERANTÖRER                   
000300*                                 ORDERHUVUD INFO                         
000400*                                 FYSISK NYCKEL: IDPRODNR                 
000500     03 PUDH-IDPRODNR        PIC S9(7)           COMP-3.                  
000600*                                 PRODUKTIONSNUMMER                       
000700*                                 PRODUCTION NUMBER                       
000800     03 PUDH-DASKEPPN        PIC 9(8).                                    
000900*                                 SKEPPNINGSDATUM  (ÅÅÅÅMMDD)             
001000*                                 SHIPPING DATE    (YYYYMMDD)             
001100     03 PUDH-DASNDDAT        PIC 9(8).                                    
001200*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
001300*                                 SHIPPING DATE   (YYYYMMDD)              
001400     03 PUDH-DAUTSKR         PIC 9(8).                                    
001500*                                 UTSKRIFTDATUM  (ÅÅÅÅMMDD)               
001600*                                 PRINTING DATE  (CCYYMMDD)               
001700     03 PUDH-IDDC            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 PUDH-IDDEPOT         PIC X(2).                                    
002100*                                 TRANSPORT DEPOT                         
002200*                                 TRANSPORT DEPOT                         
002300     03 PUDH-IDDISTR         PIC S9(5)           COMP-3.                  
002400*                                 DISTRIKTNUMMER                          
002500*                                 DISTRICT NUMBER                         
002600     03 PUDH-IDKUNDNR        PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER                              
002800*                                 CUSTOMER NO                             
002900     03 PUDH-IDLEVNR         PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003200     03 PUDH-IDORDNR7        PIC 9(7).                                    
003300*                                 ORDERNUMMER                             
003400*                                 ORDER NUMBER                            
003500     03 PUDH-IDORDER         PIC S9(7)           COMP-3.                  
003600*                                 VOLVO PARTS ORDERNUMMER                 
003700*                                 VOLVO PARTS ORDER NUMBER                
003800     03 PUDH-IDROUTE         PIC X.                                       
003900*                                 TRANSPORT ROUTE                         
004000*                                 TRANSPORT ROUTE                         
004100     03 PUDH-IDVAT           PIC X(17).                                   
004200*                                 MOMSREGISTRERINGSNUMMER                 
004300*                                 VAT REGISTRATION NUMBER                 
004400     03 PUDH-IDZON           PIC X(2).                                    
004500*                                 TRANSPORTVÄG (RUTT,ZON)                 
004600*                                 TRANSPORT ROUTE (ZONE)                  
004700     03 PUDH-KDFRAKT         PIC S9(3)           COMP-3.                  
004800*                                 FRAKTSÄTT DC TILL KUND                  
004900*                                 FREIGHT CODE                            
005000     03 PUDH-KDORDKL         PIC S9              COMP-3.                  
005100*                                 ORDERKLASS                              
005200*                                 ORDER CLASS                             
005300     03 PUDH-KDVIA           PIC X(2).                                    
005400*                                 KOD FöR LEVERANS VIA                    
005500*                                 CODE FOR DELIVERY VIA                   
005600     03 PUDH-TIREPDAT        PIC S9(7)           COMP-3.                  
005700*                                 REPAIR DATE                             
005800*                                 REPAIR DATE                             
005900     03 PUDH-TISNDTID        PIC S9(7)           COMP-3.                  
006000*                                 GENERELL SÄNDNINGSTID                   
006100*                                 GENERAL SHIPPING TIME                   
006200     03 PUDH-TITPO           PIC S9(7)           COMP-3.                  
006300*                                 PLANERAD ORDERDATUM                     
006400*                                 PLANNED ORDER DATE                      
006500     03 PUDH-BELAGINS-DIR    PIC X(35).                                   
006600*                                 LAGERINSTRUKTION DIREKTLEV.             
006700*                                 WAREHUSE INSTRUCTIONS                   
006800     03 PUDH-BEGMRK.                                                      
006900*                                 GODSMÄRKE                               
007000*                                 GOODS MARKING                           
007100        05 PUDH-BEGMRK-RAD1  PIC X(30).                                   
007200*                                 GODSMÄRKE  RAD1                         
007300*                                 GOODS MARKING  LINE1                    
007400        05 PUDH-BEGMRK-RAD2  PIC X(30).                                   
007500*                                 GODSMÄRKE  RAD2                         
007600*                                 GOODS MARKING  LINE2                    
007700     03 PUDH-BEGMT.                                                       
007800*                                 GODSMOTTAGARNAMN                        
007900*                                 GOODS RECEIVER NAME                     
008000        05 PUDH-BEGMT-RAD1   PIC X(35).                                   
008100*                                 GODSMOTTAGARNAMN RAD 1                  
008200*                                 GOODS RECEIVER NAME LINE 1              
008300        05 PUDH-BEGMT-RAD2   PIC X(35).                                   
008400*                                 GODSMOTTAGARNAMN RAD 2                  
008500*                                 GOODS RECEIVER NAME LINE 2              
008600     03 PUDH-ADGMT.                                                       
008700*                                 GODSMOTTAGARADRESS                      
008800*                                 GOODS RECEIVER ADDRESS                  
008900        05 PUDH-ADGMT-GATA   PIC X(35).                                   
009000*                                 GODSMOTTAGARADRESS GATA                 
009100*                                 GOODS RECEIVER ADDRESS STREET           
009200        05 PUDH-ADGMT-PADR   PIC X(35).                                   
009300*                                 GODSMOTTAGARADRESS POSTADRESS           
009400*                                 GOODS RECEIVER ADDRESS TOWN             
009500        05 PUDH-ADPOST-PNRORT REDEFINES PUDH-ADGMT-PADR.                  
009600*                                 POSTNUMMER + ORT                        
009700*                                 POSTAL CODE + CITY                      
009800           07 PUDH-ADPOSTNR  PIC X(10).                                   
009900*                                 POSTNUMMER I ADRESS                     
010000*                                 POSTAL CODE IN ADDRESS                  
010100           07 PUDH-ADCITY    PIC X(25).                                   
010200*                                 BENÄMNING PÅ STAD                       
010300*                                 CITY                                    
010400        05 PUDH-ADPOST-ORTPNR REDEFINES PUDH-ADGMT-PADR.                  
010500*                                 ORT + POSTNUMMER                        
010600*                                 CITY + POSTAL CODE                      
010700           07 PUDH-ADCITY    PIC X(25).                                   
010800*                                 BENÄMNING PÅ STAD                       
010900*                                 CITY                                    
011000           07 PUDH-ADPOSTNR  PIC X(10).                                   
011100*                                 POSTNUMMER I ADRESS                     
011200*                                 POSTAL CODE IN ADDRESS                  
011300        05 PUDH-ADGMT-LAND   PIC X(35).                                   
011400*                                 GODSMOTTAGARADRESS LAND                 
011500*                                 GOODS RECEIVER ADDRESS COUNTRY          
011600     03 PUDH-DEAL-PR-SUM.                                                 
011700*                                 DEALERPRIS (HUVUD)                      
011800        05 PUDH-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
011900*                                 ORDERVÄRDE SLUTKUNDPRIS                 
012000*                                 I LOKAL VALUTA                          
012100*                                 ORDER VALUE, CUSTOMER PRICE             
012200*                                 IN LOCAL CURRENCY                       
012300        05 PUDH-SUORDV-LOCPREL                                            
012400                             PIC S9(9)V9(2)      COMP-3.                  
012500*                                 ORDERVÄRDE PREL SLUT-                   
012600*                                 KUNDPRIS, LOKAL VALUTA                  
012700*                                 ORDER VALUE, PREL CUSTOMER              
012800*                                 PRICE IN LOCAL CURRENCY                 
012900        05 PUDH-KDVALISO     PIC X(3).                                    
013000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013100*                                 CURRENCY CODE BY ISO-STANDARD.          
013200*** END OF VILMAII-COPY LENGTH= 377 BYTES                                 
