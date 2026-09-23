000100 01  W4832501.                                                            
000200*                                 FOR REPORTING PURPOSE                   
000300*                                 W4832301 + WEEK MONTH PERIOD            
000400     03 IDPRODNR             PIC S9(7)           COMP-3.                  
000500*                                 PRODUCTION NUMBER                       
000600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
000700*                                 CASE NUMBER                             
000800     03 IDDC                 PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDDISTR              PIC S9(5)           COMP-3.                  
001100*                                 DISTRICT NUMBER                         
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 CUSTOMER NO                             
001400     03 KDFAKTYP             PIC X.                                       
001500*                                 INVOICE TYPE                            
001600     03 IDFAKT               PIC S9(7)           COMP-3.                  
001700*                                 INVOICE NO.                             
001800     03 TIFAKT               PIC S9(7)           COMP-3.                  
001900*                                 INVOICING DATE   (YYMMDD)               
002000     03 TIFAKTID             PIC S9(7)           COMP-3.                  
002100     03 KDORDKL              PIC S9              COMP-3.                  
002200*                                 ORDER CLASS                             
002300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002400*                                 FREIGHT CODE                            
002500     03 FLDIRLEV             PIC X.                                       
002600*                                 DIRECT DELIVERY ?                       
002700     03 IDLEVNR              PIC X(5).                                    
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 KDVIA                PIC X(2).                                    
003000*                                 CODE FOR DELIVERY VIA                   
003100     03 DASUPREF             PIC 9(8).                                    
003200*                                 SHIPPING DATE DIRECT SUPPLIER           
003300     03 TISUPTID             PIC S9(5)           COMP-3.                  
003400*                                 SHIPPING TIME DIRECT SUPPLIER           
003500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
003600*                                 PRINTING DATE  (YYMMDD)                 
003700     03 TIUTSTID             PIC S9(7)           COMP-3.                  
003800*                                 TIME OF PRINTING (HHMMSS)               
003900     03 TILASTN              PIC S9(7)           COMP-3.                  
004000*                                 LOADING DATE           (YYMMDD)         
004100     03 TILASTID             PIC S9(7)           COMP-3.                  
004200     03 IDTRPTNR             PIC S9(3)           COMP-3.                  
004300*                                 TRANSPORT IDENTITY                      
004400     03 IDLBBET              PIC X(12).                                   
004500*                                 TRAILER NUMBER                          
004600     03 IDSHIPM              PIC 9(7).                                    
004700*                                 SHIPMENT NO                             
004800     03 IDLASTN              PIC S9(7)           COMP-3.                  
004900*                                 LOADING NO.                             
005000     03 DIKOLLIL             PIC S9(5)           COMP-3.                  
005100*                                 CASE LENGTH                             
005200     03 DIKOLLIB             PIC S9(3)           COMP-3.                  
005300*                                 CASE WIDTH                              
005400     03 DIKOLLIH             PIC S9(3)           COMP-3.                  
005500*                                 CASE HEIGHT                             
005600     03 KDKOLLI              PIC X(8).                                    
005700*                                 KOLLI CODE                              
005800     03 KDFARLIG-KOLLI       PIC S9              COMP-3.                  
005900*                                 CODE FOR DANG GOODS IN A CASE           
006000     03 KVFLAMP-KOLLI        PIC S9(2)V9(1)      COMP-3.                  
006100*                                 FLASH POINT FOR A CASE                  
006200     03 FG-PSN               OCCURS 10 TIMES.                             
006300        05 IDPSN             PIC 9(3).                                    
006400*                                 PROPER SHIPPING NAME                    
006500        05 VKART-FG          PIC S9(7)           COMP-3.                  
006600*                                 NET WEIGHT EXPLOSIVES                   
006700        05 VLFG              PIC S9(4)V9(3)      COMP-3.                  
006800*                                 VOLUME DANGEROUS GOODS                  
006900     03 SUEQFG               PIC S9(3)V9(4)      COMP-3.                  
007000*                                 EQ VALUE DANGEROUS GODS                 
007100     03 KVFALRAD             PIC S9(5)           COMP-3.                  
007200*                                 NUMBER OF LINES DANGEROUS GOODS         
007300     03 KVORDRAD             PIC S9(5)           COMP-3.                  
007400*                                 NUMBER OF ORDER LINES                   
007500     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
007600*                                 ORDER VALUE PER CASE                    
007700     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
007800*                                 ORDER WEIGHT NET PER CASE               
007900     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
008000*                                 ORDER WEIGHT GROSS PER CASE             
008100     03 VLORDBTO-KOLLI       PIC S9(4)V9(3)      COMP-3.                  
008200*                                 ORDER VOL GR/CASE                       
008300     03 FLLDCKND             PIC X.                                       
008400*                                 FL LDC CUSTOMER                         
008500     03 IDLANDX2             PIC X(2).                                    
008600*                                 2-LETTER CODE FOR COUNTRY               
008700     03 TIAAVV-LASTN         PIC S9(5)           COMP-3.                  
008800*                                 YEAR - WEEK  (YYWW)                     
008900     03 TIAAMM-LASTN         PIC S9(5)           COMP-3.                  
009000*                                 YEAR - MONTH (YYMM)                     
009100     03 TIAARP-LASTN         PIC S9(5)           COMP-3.                  
009200*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
009300*                                 12 PER YEAR                             
009400     03 TIAA-LASTN           PIC S9(3)           COMP-3.                  
009500*                                 YEAR  (YY)                              
009600*** END OF VILMAII-COPY LENGTH= 262 BYTES                                 
