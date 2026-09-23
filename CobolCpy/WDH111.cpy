000100 01  INV-WDH111.                                                          
000200*                                 INVENTERINGSREGISTER                    
000300*                                 INVENTERINGS-INFO                       
000400*                                 FYSISK-NYCKEL: WDH111KY                 
000500*                                  (IDDC + KDINVKAT + TISEGKEY+           
000600*                                  (DAREGDAT-9KOMPL           )           
000700     03 INV-IDDC             PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 INV-KDINVKAT         PIC S9(3)           COMP-3.                  
001100*                                 INVENTERINGSKATEGORI                    
001200*                                 STOCKTAKING CATEGORY                    
001300     03 INV-TISEGKEY         PIC S9(9)           COMP-3.                  
001400*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
001500*                                 TECHNICAL SEGMENT KEY                   
001600     03 INV-DAREGDAT-SORT    PIC 9(8).                                    
001700*                                 DATUM FÖR SORTERING                     
001800*                                 DATES FOR SORTING                       
001900     03 INV-ADART.                                                        
002000*                                 ARTIKELADRESS I LAGRET                  
002100*                                 PARTS-ADRESS                            
002200        05 INV-ADLAGOMR      PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500        05 INV-ADGANG        PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800        05 INV-ADPLATS       PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100     03 INV-FLINVBEH         PIC X.                                       
003200*                                 INV. ONLINE JUSTERING                   
003300*                                 STOCKTAKING ADJUSTMENT ONLINE           
003400     03 INV-FLINVSKR         PIC X.                                       
003500*                                 INVENTERINGSANMODAN UTSKRIVEN           
003600*                                 STOCKTAKING ORDER WRITTEN               
003700     03 INV-FILLER2          PIC X.                                       
003800     03 INV-FLINV2B          PIC X.                                       
003900*                                 INV. FYSISKA AVVIKELSER CDC+SDC         
004000*                                 STOCKTAKING PHYSICAL DEVIATIONS         
004100     03 INV-FLINV2C          PIC X.                                       
004200*                                 INVENTERINGSJUSTERINGAR CDC+SDC         
004300*                                 STOCKTAKING ADJUSTMENTS                 
004400     03 INV-FLINV2D          PIC X.                                       
004500*                                 INVENTERING VECKORAPPORTTRANSAR         
004600*                                 STOCKTAKING WEEKLEY REPORT TRAN         
004700     03 INV-FLINV3E          PIC X.                                       
004800*                                 INVENTERING ANTAL PER PERIOD            
004900*                                 STOCKTAKING NUMBER PER PERIOD           
005000     03 INV-FLINV4N          PIC X.                                       
005100*                                 INVENTERING AVVIKELSER NDC              
005200*                                 STOCKTAKING PHYSICAL DIVIATIONS         
005300     03 INV-FLINV4P          PIC X.                                       
005400*                                 INVENTERING JUSTERINGAR NDC             
005500*                                 STOCKTAKING ADJUSTMENTS NDC             
005600     03 INV-FLINV4R          PIC X.                                       
005700*                                 INVENTERING VECKORAPPORT NDC            
005800*                                 STOCKTAKING WEEKLEY REPORT TRAN         
005900     03 INV-KDINVKAT-OLD     PIC 9(2).                                    
006000*                                 SPARAD INVENTERINGSKATEGORI             
006100*                                 SAVED STOCKTAKING CATEGORY              
006200     03 INV-FLINV85          PIC X.                                       
006300*                                 INV. ERSÄTTNINGSINVENTERING             
006400*                                 SUBSTITUTE STOCK AMOUNT LIST            
006500     03 INV-FILLER1          PIC X.                                       
006600     03 INV-IDFKNGRP         PIC S9(5)           COMP-3.                  
006700*                                 FUNKTIONSGRUPP                          
006800*                                 FUNCTION GROUP                          
006900     03 INV-KDINVPRIO        PIC S9              COMP-3.                  
007000*                                 INVENTERING PRIORITET                   
007100*                                 STOCKTAKING PRIORITY                    
007200     03 INV-KDVVKL           PIC S9              COMP-3.                  
007300*                                 VOLYMVÄRDESKLASS                        
007400*                                 VOLUME VALUE CLASS                      
007500     03 INV-TEINVANM         PIC X(25).                                   
007600*                                 INVENTERINGSANMÄRKNING                  
007700*                                 STOCKTAKING COMMENT                     
007800     03 INV-IDPRTINV.                                                     
007900*                                 PRINTNINGSIDENTITET                     
008000*                                 PRINTIDENTITY                           
008100        05 INV-IDPRTOMG      PIC S9              COMP-3.                  
008200*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
008300*                                 PRINT ROUND OF AUT.ADJUSTMENT           
008400        05 INV-IDLOPNR       PIC S9(5)           COMP-3.                  
008500*                                 LÖPNUMMER          IDLOPNR-002          
008600     03 INV-KDPRODSL         PIC S9(3)           COMP-3.                  
008700*                                 PRODUKTSLAG                             
008800*                                 PRODUCT GROUP                           
008900     03 INV-KDPSLLOC         PIC 9(2).                                    
009000*                                 PRODUKTSLAG LOKALT                      
009100*                                 PRODUCT GROUP LOCAL                     
009200     03 INV-KVJUSTKV         PIC S9(7)           COMP-3.                  
009300*                                 JUSTERAD KVANTITET                      
009400*                                 ADJUSTED QUANTITY                       
009500     03 INV-KVAKS-OLD        PIC S9(7)           COMP-3.                  
009600*                                 ANKOMSTSALDO FÖRE ÄNDRING               
009700*                                 ADVICED BALANCE BEFORE CHANGE           
009800     03 INV-KVEFRS-OLD       PIC S9(7)           COMP-3.                  
009900*                                 EJ FAKTURERAT ANTAL FÖRE ÄNDR.          
010000*                                 NOT INVOICED QTY BEFORE CHANGE          
010100     03 INV-KVLS-OLD         PIC S9(7)           COMP-3.                  
010200*                                 LAGERSALDO FÖRE ÄNDRING                 
010300*                                 STOCK BALANCE BEFORE CHANGE             
010400     03 INV-DAREGDAT         PIC S9(9)           COMP-3.                  
010500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
010600*                                 REGISTRATION DATE (YYYYMMDD)            
010700     03 INV-DAREGDAT-PR1     PIC S9(9)           COMP-3.                  
010800*                                 DATUM FÖR FÖRSTA PRINTNING              
010900*                                 DATE FOR FIRST PRINTING                 
011000     03 INV-DAREGDAT-PR2     PIC S9(9)           COMP-3.                  
011100*                                 DATUM FÖR ANDRA  PRINTNING              
011200*                                 DATE FOR SECOND PRINT                   
011300     03 INV-DAREGDAT-PR3     PIC S9(9)           COMP-3.                  
011400*                                 DATUM FÖR TREDJE PRINTNING              
011500*                                 DATE FOR THIRD PRINT                    
011600     03 INV-DAREGDAT-CRE     PIC S9(9)           COMP-3.                  
011700*                                 DATUM NÄR INVENTERING PÅBÖRJAS          
011800*                                 CREATION DATE                           
011900     03 INV-FILLER           PIC X(33).                                   
012000*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
