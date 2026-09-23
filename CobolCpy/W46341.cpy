000100 01  W46341.                                                              
000200*                                 DIRECT DELIVERIES - DDGS                
000300*                                 FOLLOW-UP OF DIRECT DELIVERY            
000400*                                 EVENTS                                  
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800*                                 RECORD TYPE                             
000900     03 IDPRODNR             PIC 9(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 IDLEVNR              PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 IDSUPREF             PIC X(10).                                   
001900*                                 LEVERANTöRSREF.                         
002000*                                 SUPPLIER REF.                           
002100     03 IDDISTR              PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 IDKUNDNR             PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700     03 IDORDNR7             PIC 9(7).                                    
002800*                                 ORDERNUMMER                             
002900*                                 ORDER NUMBER                            
003000     03 IDKOLLI              PIC 9(5).                                    
003100*                                 KOLLINUMMER                             
003200*                                 CASE NUMBER                             
003300     03 IDRADNR              PIC 9(4).                                    
003400*                                 RADNUMMER                               
003500*                                 LINE NO                                 
003600     03 IDARTNR              PIC 9(8).                                    
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900     03 BERADREF             PIC X(10).                                   
004000*                                 KUNDENS RADREFERENS                     
004100*                                 CUSTOMERS ITEM REF.                     
004200     03 DABEKDAT             PIC 9(8).                                    
004300*                                 ORDERBEKRÄFTELSEDATUM                   
004400*                                                                         
004500*                                 ORDERCONFIRMATION-DATE                  
004600     03 DAFAKT               PIC 9(8).                                    
004700*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
004800*                                 INVOICING DATE   (YYYYMMDD)             
004900     03 DALEVDAT             PIC 9(8).                                    
005000*                                 FÖRSENAT LEVERANSDATUM                  
005100*                                 DELAYED DELIVERY-DATE                   
005200     03 DAREGDAT             PIC 9(8).                                    
005300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005400*                                 REGISTRATION DATE (YYYYMMDD)            
005500     03 DAPACKN              PIC 9(8).                                    
005600*                                 PACKNINGSDATUM         (SSÅÅMMD         
005700*                                 D)                                      
005800*                                 PACKING DATE           (CCYYMMD         
005900*                                 D)                                      
006000     03 DASKEPPN             PIC 9(8).                                    
006100*                                 SKEPPNINGSDATUM  (ÅÅÅÅMMDD)             
006200*                                 SHIPPING DATE    (YYYYMMDD)             
006300     03 DASNDDAT             PIC 9(8).                                    
006400*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
006500*                                 SHIPPING DATE   (YYYYMMDD)              
006600     03 DASUPREF             PIC 9(8).                                    
006700*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
006800*                                 SHIPPING DATE DIRECT SUPPLIER           
006900     03 TIBEKR               PIC 9(6).                                    
007000*                                 KLOCKSLAG FÖR ORDERBEKRÄFTELSE          
007100*                                 TIME FOR ORDER-CONFIRMATION             
007200     03 TIPACTID             PIC 9(6).                                    
007300*                                 PACKNINGSTID  TTMMSS                    
007400*                                 PACKING TIME  HHMMSS                    
007500     03 TIREGTID             PIC 9(6).                                    
007600*                                 REGISTRERINGSTID                        
007700*                                 GENERAL REGISTRATION TIME               
007800     03 TISNDTID             PIC 9(6).                                    
007900*                                 GENERELL SÄNDNINGSTID                   
008000*                                 GENERAL SHIPPING TIME                   
008100     03 TISUPTID             PIC 9(4).                                    
008200*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
008300*                                 SHIPPING TIME DIRECT SUPPLIER           
008400     03 KDORDBEK             PIC 9(2).                                    
008500*                                 ORDERBEKRÄFTELSEKOD                     
008600*                                 ORDERCONFIMATIONCODE                    
008700     03 KDORDKL              PIC 9.                                       
008800*                                 ORDERKLASS                              
008900*                                 ORDER CLASS                             
009000     03 KDVIA                PIC X(2).                                    
009100*                                 KOD FöR LEVERANS VIA                    
009200*                                 CODE FOR DELIVERY VIA                   
009300     03 KVANTAL              PIC 9(6).                                    
009400*                                 ANTAL                                   
009500*                                 NUMBER                                  
009600     03 FILLER               PIC X(26).                                   
009700*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
