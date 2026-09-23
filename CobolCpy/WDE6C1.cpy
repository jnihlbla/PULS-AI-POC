000100 01  SEQC-WDE6C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE611             
000300*                                 EXIT: INDEX FINNS NÄR                   
000400*                                 FLUTLAST  = J                           
000500*                                 FYSISK NYCKEL: WDE6C1KY                 
000600*                                  (IDTRPTNR, DARFS, ADKOLLI,             
000700*                                   IDDISTR, IDKUNDNR, IDPRODNR,          
000800*                                   IDKOLLIF, IDKOLLI)                    
000900*                                 SECONDARY KEY: WDE6CSEQ                 
001000*                                  (IDTRPTNR, DARFS, ADKOLLI)             
001100     03 SEQC-IDTRPTNR        PIC S9(3)           COMP-3.                  
001200*                                 TRANSPORTIDENTITET                      
001300*                                 TRANSPORT IDENTITY                      
001400     03 SEQC-DARFS           PIC 9(12).                                   
001500*                                 KLART FÖR TRANSPORT                     
001600*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
001700     03 SEQC-ADKOLLI.                                                     
001800*                                 KOLLI-ADRESS                            
001900*                                 PALLET LOCATION                         
002000        05 SEQC-ADCLGEO.                                                  
002100*                                 IDDC + GEO ADRESS                       
002200*                                 IDDC + GEO AREA                         
002300           07 SEQC-IDDC      PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600           07 SEQC-ADFLGEO   PIC X(3).                                    
002700*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002800*                                 GEOGRAPHIC AREA                         
002900        05 SEQC-ADFLOMR      PIC S9(3)           COMP-3.                  
003000*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003100*                                 DELIVERY AREA                           
003200        05 SEQC-ADRUTNIV     PIC S9(3)           COMP-3.                  
003300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003400*                                 SQUARE/LEVEL IN DEL. AREA               
003500        05 SEQC-ADVMODUL     PIC S9(3)           COMP-3.                  
003600*                                 VÄNSTER-MODUL                           
003700*                                 LEFT-MODUL                              
003800     03 SEQC-IDDISTR         PIC S9(5)           COMP-3.                  
003900*                                 DISTRIKTNUMMER                          
004000*                                 DISTRICT NUMBER                         
004100     03 SEQC-IDKUNDNR        PIC S9(7)           COMP-3.                  
004200*                                 KUNDNUMMER                              
004300*                                 CUSTOMER NO                             
004400     03 SEQC-IDPRODNR        PIC S9(7)           COMP-3.                  
004500*                                 PRODUKTIONSNUMMER                       
004600*                                 PRODUCTION NUMBER                       
004700     03 SEQC-IDKOLLI-FLER    PIC S9(5)           COMP-3.                  
004800*                                 KOLLINUMMER                             
004900*                                 CASE NUMBER                             
005000     03 SEQC-IDKOLLI         PIC S9(5)           COMP-3.                  
005100*                                 KOLLINUMMER                             
005200*                                 CASE NUMBER                             
005300     03 SEQC-ADHMODUL        PIC S9(3)           COMP-3.                  
005400*                                 HÖGER-MODUL                             
005500*                                 RIGHT-MODUL                             
005600     03 SEQC-KDFARLIG-KOLLI  PIC S9              COMP-3.                  
005700*                                 KOD FÖR FARLIGT GODS I KOLLI            
005800*                                 CODE FOR DANG GOODS IN A CASE           
005900     03 SEQC-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
006000*                                 ORDERVIKT BRUTTO PER KOLLI              
006100*                                 ORDER WEIGHT GROSS PER CASE             
006200     03 SEQC-VLORDBTO-KOLLI  PIC S9(4)V9(3)      COMP-3.                  
006300*                                 ORDERVOLYM BRUTTO KOLLI                 
006400*                                 ORDER VOL GR/CASE                       
006500     03 SEQC-KDORDKL         PIC S9              COMP-3.                  
006600*                                 ORDERKLASS                              
006700*                                 ORDER CLASS                             
006800     03 SEQC-FLAUTFAK        PIC X.                                       
006900*                                 AUTOMATFAKTURERING ?                    
007000*                                 AUTOMATIC INVOICING ?                   
007100     03 SEQC-KDKOLLI         PIC X(8).                                    
007200*                                 KOLLIKOD                                
007300*                                 KOLLI CODE                              
007400     03 SEQC-KDKOLSTA        PIC S9              COMP-3.                  
007500*                                 KOLLISTATUS                             
007600*                                 CASE STATUS                             
007700     03 SEQC-IDPSN           OCCURS 2 TIMES                               
007800                             PIC 9(3).                                    
007900*                                 PROPER SHIPPING NAME                    
008000*                                 PROPER SHIPPING NAME                    
008100*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
