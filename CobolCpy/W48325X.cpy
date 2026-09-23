000100 01  W48325X.                                                             
000200*                                 FOR REPORTING PURPOSE                   
000300*                                                                         
000400*                                 W48323X + WEEK MONTH PERIOD YEA         
000500*                                 R                                       
000600*                                 FILE IN EDITABLE FORMAT                 
000700*                                                                         
000800     03 IDPRODNR             PIC Z(6)9                                    
000900                             VALUE ZEROS.                                 
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200     03 IDKOLLI              PIC Z(4)9                                    
001300                             VALUE ZEROS.                                 
001400*                                 KOLLINUMMER                             
001500*                                 CASE NUMBER                             
001600     03 IDDC                 PIC X(2)                                     
001700                             VALUE SPACES.                                
001800*                                 IDENTIFIERARE LAGER                     
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 IDDISTR              PIC Z(3)9                                    
002100                             VALUE ZEROS.                                 
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 IDKUNDNR             PIC Z(5)9                                    
002500                             VALUE ZEROS.                                 
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800     03 KDFAKTYP             PIC X                                        
002900                             VALUE SPACE.                                 
003000*                                 FAKTURATYP                              
003100*                                 INVOICE TYPE                            
003200     03 IDFAKT               PIC Z(6)9                                    
003300                             VALUE ZEROS.                                 
003400*                                 FAKTURANUMMER                           
003500*                                 INVOICE NO.                             
003600     03 TIFAKT               PIC 9(6)                                     
003700                             VALUE ZEROS.                                 
003800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003900*                                 INVOICING DATE   (YYMMDD)               
004000     03 TIFAKTID             PIC 9(6)                                     
004100                             VALUE ZEROS.                                 
004200*                                 FAKTURERINGSTID                         
004300     03 KDORDKL              PIC 9                                        
004400                             VALUE ZERO.                                  
004500*                                 ORDERKLASS                              
004600*                                 ORDER CLASS                             
004700     03 KDFRAKT              PIC Z9                                       
004800                             VALUE ZEROS.                                 
004900*                                 FRAKTSÄTT DC TILL KUND                  
005000*                                 FREIGHT CODE                            
005100     03 FLDIRLEV             PIC X                                        
005200                             VALUE SPACE.                                 
005300*                                 DIREKTLEVERANS ?                        
005400*                                 DIRECT DELIVERY ?                       
005500     03 IDLEVNR              PIC X(5)                                     
005600                             VALUE SPACES.                                
005700*                                 LEVERANTÖRNUMMER                        
005800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005900     03 KDVIA                PIC X(2)                                     
006000                             VALUE SPACES.                                
006100*                                 KOD FöR LEVERANS VIA                    
006200*                                 CODE FOR DELIVERY VIA                   
006300     03 DASUPREF             PIC 9(8)                                     
006400                             VALUE ZEROS.                                 
006500*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
006600*                                 SHIPPING DATE DIRECT SUPPLIER           
006700     03 TISUPTID             PIC 9(4)                                     
006800                             VALUE ZEROS.                                 
006900*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
007000*                                 SHIPPING TIME DIRECT SUPPLIER           
007100     03 TIUTSKR              PIC 9(6)                                     
007200                             VALUE ZEROS.                                 
007300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
007400*                                 PRINTING DATE  (YYMMDD)                 
007500     03 TIUTSTID             PIC 9(6)                                     
007600                             VALUE ZEROS.                                 
007700*                                 UTSKRIFTSTID (TTMMSS)                   
007800*                                 TIME OF PRINTING (HHMMSS)               
007900     03 TILASTN              PIC 9(6)                                     
008000                             VALUE ZEROS.                                 
008100*                                 LASTNINGSDATUM         (ÅÅMMDD)         
008200*                                 LOADING DATE           (YYMMDD)         
008300     03 TILASTID             PIC 9(6)                                     
008400                             VALUE ZEROS.                                 
008500*                                 LASTNINGSTID                            
008600     03 IDTRPTNR             PIC Z(2)9                                    
008700                             VALUE ZEROS.                                 
008800*                                 TRANSPORTIDENTITET                      
008900*                                 TRANSPORT IDENTITY                      
009000     03 IDLBBET              PIC X(12)                                    
009100                             VALUE SPACES.                                
009200*                                 LASTBÄRARBETECKNING                     
009300*                                 TRAILER NUMBER                          
009400     03 IDSHIPM              PIC Z(6)9                                    
009500                             VALUE ZEROS.                                 
009600*                                 SKEPPNINGSNUMMER                        
009700*                                 SHIPMENT NO                             
009800     03 IDLASTN              PIC Z(6)9                                    
009900                             VALUE ZEROS.                                 
010000*                                 LASTNINGSNUMMER                         
010100*                                 LOADING NO.                             
010200     03 DIKOLLIL             PIC Z(3)9                                    
010300                             VALUE ZEROS.                                 
010400*                                 KOLLI-LÄNGD                             
010500*                                 CASE LENGTH                             
010600     03 DIKOLLIB             PIC Z(2)9                                    
010700                             VALUE ZEROS.                                 
010800*                                 KOLLI-BREDD                             
010900*                                 CASE WIDTH                              
011000     03 DIKOLLIH             PIC Z(2)9                                    
011100                             VALUE ZEROS.                                 
011200*                                 KOLLI-HÖJD                              
011300*                                 CASE HEIGHT                             
011400     03 KDKOLLI              PIC X(8)                                     
011500                             VALUE SPACES.                                
011600*                                 KOLLIKOD                                
011700*                                 KOLLI CODE                              
011800     03 KDFARLIG-KOLLI       PIC 9                                        
011900                             VALUE ZERO.                                  
012000*                                 KOD FÖR FARLIGT GODS I KOLLI            
012100*                                 CODE FOR DANG GOODS IN A CASE           
012200     03 KVFLAMP-KOLLI        PIC Z9.9                                     
012300                             VALUE ZEROS.                                 
012400*                                 KOLLITS FLAMPUNKT                       
012500*                                 FLASH POINT FOR A CASE                  
012600     03 FG-PSN               OCCURS 10 TIMES.                             
012700        05 IDPSN             PIC 9(3)                                     
012800                             VALUE ZEROS.                                 
012900*                                 PROPER SHIPPING NAME                    
013000*                                 PROPER SHIPPING NAME                    
013100        05 VKART-FG          PIC Z(6)9                                    
013200                             VALUE ZEROS.                                 
013300*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
013400*                                 NET WEIGHT EXPLOSIVES                   
013500        05 VLFG              PIC Z(3)9.9(3)                               
013600                             VALUE ZEROS.                                 
013700*                                 VOLYM FARLIGT GODS                      
013800*                                 VOLUME DANGEROUS GOODS                  
013900     03 SUEQFG               PIC Z(2)9.9(4)                               
014000                             VALUE ZEROS.                                 
014100*                                 EQ-VÄRDE FARLIGT GODS                   
014200*                                 EQ VALUE DANGEROUS GODS                 
014300     03 KVFALRAD             PIC Z(4)9                                    
014400                             VALUE ZEROS.                                 
014500*                                 ANTAL RADER MED FARLIGT GODS            
014600*                                 NUMBER OF LINES DANGEROUS GOODS         
014700     03 KVORDRAD             PIC Z(4)9                                    
014800                             VALUE ZEROS.                                 
014900*                                 ANTAL ORDERRADER                        
015000*                                 NUMBER OF ORDER LINES                   
015100     03 SUORDV-KOLLI         PIC Z(8)9.9(2)                               
015200                             VALUE ZEROS.                                 
015300*                                 VARUVÄRDE PER KOLLI                     
015400*                                 ORDER VALUE PER CASE                    
015500     03 VKORDNTO-KOLLI       PIC Z(5)9.9                                  
015600                             VALUE ZEROS.                                 
015700*                                 ORDERVIKT NETTO PER KOLLI               
015800*                                 ORDER WEIGHT NET PER CASE               
015900     03 VKORDBTO-KOLLI       PIC Z(5)9.9                                  
016000                             VALUE ZEROS.                                 
016100*                                 ORDERVIKT BRUTTO PER KOLLI              
016200*                                 ORDER WEIGHT GROSS PER CASE             
016300     03 VLORDBTO-KOLLI       PIC Z(3)9.9(3)                               
016400                             VALUE ZEROS.                                 
016500*                                 ORDERVOLYM BRUTTO KOLLI                 
016600*                                 ORDER VOL GR/CASE                       
016700     03 FLLDCKND             PIC X                                        
016800                             VALUE SPACE.                                 
016900*                                 FL LDC-KUND                             
017000*                                 FL LDC CUSTOMER                         
017100     03 IDLANDX2             PIC X(2)                                     
017200                             VALUE SPACES.                                
017300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
017400*                                 2-LETTER CODE FOR COUNTRY               
017500     03 TIAAVV-LASTN         PIC 9(4)                                     
017600                             VALUE ZEROS.                                 
017700*                                 ÅR - VECKA  (ÅÅVV)                      
017800*                                 YEAR - WEEK  (YYWW)                     
017900     03 TIAAMM-LASTN         PIC 9(4)                                     
018000                             VALUE ZEROS.                                 
018100*                                 ÅR - MÅNAD (ÅÅMM)                       
018200*                                 YEAR - MONTH (YYMM)                     
018300     03 TIAARP-LASTN         PIC 9(4)                                     
018400                             VALUE ZEROS.                                 
018500*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
018600*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
018700*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
018800*                                 12 PER YEAR, ALSO LOGISTICS PER         
018900     03 TIAA-LASTN           PIC 9(2)                                     
019000                             VALUE ZEROS.                                 
019100*                                 ÅR    (ÅÅ)                              
019200*                                 YEAR  (YY)                              
019300*** END OF VILMAII-COPY LENGTH= 394 BYTES                                 
