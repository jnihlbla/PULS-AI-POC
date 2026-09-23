000100 01  W4832301.                                                            
000200*                                 FOR REPORTING PURPOSE                   
000300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
000400*                                 PRODUCTION NUMBER                       
000500     03 IDKOLLI              PIC S9(5)           COMP-3.                  
000600*                                 CASE NUMBER                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRICT NUMBER                         
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 CUSTOMER NO                             
001300     03 KDFAKTYP             PIC X.                                       
001400*                                 INVOICE TYPE                            
001500     03 IDFAKT               PIC S9(7)           COMP-3.                  
001600*                                 INVOICE NO.                             
001700     03 TIFAKT               PIC S9(7)           COMP-3.                  
001800*                                 INVOICING DATE   (YYMMDD)               
001900     03 TIFAKTID             PIC S9(7)           COMP-3.                  
002000     03 KDORDKL              PIC S9              COMP-3.                  
002100*                                 ORDER CLASS                             
002200     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002300*                                 FREIGHT CODE                            
002400     03 FLDIRLEV             PIC X.                                       
002500*                                 DIRECT DELIVERY ?                       
002600     03 IDLEVNR              PIC X(5).                                    
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800     03 KDVIA                PIC X(2).                                    
002900*                                 CODE FOR DELIVERY VIA                   
003000     03 DASUPREF             PIC 9(8).                                    
003100*                                 SHIPPING DATE DIRECT SUPPLIER           
003200     03 TISUPTID             PIC S9(5)           COMP-3.                  
003300*                                 SHIPPING TIME DIRECT SUPPLIER           
003400     03 TIUTSKR              PIC S9(7)           COMP-3.                  
003500*                                 PRINTING DATE  (YYMMDD)                 
003600     03 TIUTSTID             PIC S9(7)           COMP-3.                  
003700*                                 TIME OF PRINTING (HHMMSS)               
003800     03 TILASTN              PIC S9(7)           COMP-3.                  
003900*                                 LOADING DATE           (YYMMDD)         
004000     03 TILASTID             PIC S9(7)           COMP-3.                  
004100     03 IDTRPTNR             PIC S9(3)           COMP-3.                  
004200*                                 TRANSPORT IDENTITY                      
004300     03 IDLBBET              PIC X(12).                                   
004400*                                 TRAILER NUMBER                          
004500     03 IDSHIPM              PIC 9(7).                                    
004600*                                 SHIPMENT NO                             
004700     03 IDLASTN              PIC S9(7)           COMP-3.                  
004800*                                 LOADING NO.                             
004900     03 DIKOLLIL             PIC S9(5)           COMP-3.                  
005000*                                 CASE LENGTH                             
005100     03 DIKOLLIB             PIC S9(3)           COMP-3.                  
005200*                                 CASE WIDTH                              
005300     03 DIKOLLIH             PIC S9(3)           COMP-3.                  
005400*                                 CASE HEIGHT                             
005500     03 KDKOLLI              PIC X(8).                                    
005600*                                 KOLLI CODE                              
005700     03 KDFARLIG-KOLLI       PIC S9              COMP-3.                  
005800*                                 CODE FOR DANG GOODS IN A CASE           
005900     03 KVFLAMP-KOLLI        PIC S9(2)V9(1)      COMP-3.                  
006000*                                 FLASH POINT FOR A CASE                  
006100     03 FG-PSN               OCCURS 10 TIMES.                             
006200        05 IDPSN             PIC 9(3).                                    
006300*                                 PROPER SHIPPING NAME                    
006400        05 VKART-FG          PIC S9(7)           COMP-3.                  
006500*                                 NET WEIGHT EXPLOSIVES                   
006600        05 VLFG              PIC S9(4)V9(3)      COMP-3.                  
006700*                                 VOLUME DANGEROUS GOODS                  
006800     03 SUEQFG               PIC S9(3)V9(4)      COMP-3.                  
006900*                                 EQ VALUE DANGEROUS GODS                 
007000     03 KVFALRAD             PIC S9(5)           COMP-3.                  
007100*                                 NUMBER OF LINES DANGEROUS GOODS         
007200     03 KVORDRAD             PIC S9(5)           COMP-3.                  
007300*                                 NUMBER OF ORDER LINES                   
007400     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
007500*                                 ORDER VALUE PER CASE                    
007600     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
007700*                                 ORDER WEIGHT NET PER CASE               
007800     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
007900*                                 ORDER WEIGHT GROSS PER CASE             
008000     03 VLORDBTO-KOLLI       PIC S9(4)V9(3)      COMP-3.                  
008100*                                 ORDER VOL GR/CASE                       
008200     03 FLLDCKND             PIC X.                                       
008300*                                 FL LDC CUSTOMER                         
008400     03 IDLANDX2             PIC X(2).                                    
008500*                                 2-LETTER CODE FOR COUNTRY               
008600*** END OF VILMAII-COPY LENGTH= 251 BYTES                                 
