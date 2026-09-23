000100 01  RIS-W461RIS0-CTX.                                                    
000200*                                 PARTNO. INFORM.  TRANS TO               
000300*                                 IMPORTER   RECORD TYPE  RIS             
000400     03 RIS-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIS-IDARTNR          PIC 9(9).                                    
000700*                                 PART NUMBER                             
000800     03 RIS-REKSIFFR         PIC 9.                                       
000900*                                 PART NO CHECK DIGIT                     
001000     03 RIS-IDFKNGRP         PIC 9(4).                                    
001100*                                 FUNCTION GROUP                          
001200     03 RIS-KDSRA            PIC 9(2).                                    
001300*                                 SRA CODE                                
001400     03 RIS-KVQPACK-1        PIC 9(5).                                    
001500*                                 QUANTITY IN BULK PACK Q1                
001600     03 RIS-KDCLAGER         PIC 9.                                       
001700*                                 CENTRAL WAREHOUSE CODE                  
001800     03 RIS-KDARTURS         PIC X(2).                                    
001900*                                 COUNTRY OF ORIGIN                       
002000     03 RIS-KDPRODSL         PIC 9(2).                                    
002100*                                 PRODUCT GROUP                           
002200     03 RIS-VLARTNTO         PIC 9(8)V9(1).                               
002300*                                 PART NET VOLUME    (CM3)                
002400     03 RIS-VKART            PIC 9(7).                                    
002500*                                 PART WEIGHT (G)                         
002600     03 RIS-KDVSOP           PIC 9(3).                                    
002700*                                 VSOP-CODE                               
002800     03 RIS-IDSTATNR         PIC 9(9).                                    
002900*                                 STATISTICAL NO.                         
003000     03 RIS-PRARTBTO-EXP     PIC 9(7)V9(2).                               
003100*                                 GROSS-PRICE EXPORT                      
003200*                                  (FOB-GROSS)                            
003300     03 RIS-FLMILART         PIC X.                                       
003400     03 RIS-KDSORT           PIC X(2).                                    
003500*                                 UNIT OF MEASURE                         
003600     03 RIS-KDERS            PIC 9(2).                                    
003700*                                 SUPERSESSION CODE                       
003800     03 RIS-KDBPSR           PIC 9.                                       
003900*                                 BASIC PART STOCK RECOMMENDATION         
004000     03 RIS-KDBBCL           PIC 9.                                       
004100*                                 RETURNABLE PART                         
004200     03 RIS-IDLEVNR          PIC X(5).                                    
004300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004400     03 RIS-KDAGE            PIC X.                                       
004500*                                 AGE-CODE                                
004600     03 RIS-FILLERX1         PIC X.                                       
004700*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
