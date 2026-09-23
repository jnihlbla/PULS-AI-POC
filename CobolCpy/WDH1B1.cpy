000100 01  SEQB-WDH1B1.                                                         
000200*                                 SEKUNDƒRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 INDEX FINNS NƒR IDPRTOMG > 0            
000500*                                 FYSISK-NYCKEL: WDH1B1KY                 
000600*                                  (IDDC + DAREGDAT + ADLAGOMR +          
000700*                                   ADGANG + ADPLATS + IDPRTINV +         
000800*                                   + IDARTNR + KDINVKAT                  
000900*                                 SEKUNDƒR NYCKEL: WDH1BSEQ               
001000*                                  (IDDC + DAREGDAT + ADLAGOMR +          
001100*                                  ADGANG + ADPLATS + IDPRTINV)           
001200     03 SEQB-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQB-DAREGDAT        PIC S9(9)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001700*                                 REGISTRATION DATE (YYYYMMDD)            
001800     03 SEQB-ADART.                                                       
001900*                                 ARTIKELADRESS I LAGRET                  
002000*                                 PARTS-ADRESS                            
002100        05 SEQB-ADLAGOMR     PIC S9(3)           COMP-3.                  
002200*                                 LAGEROMR≈DE                             
002300*                                 AREA                                    
002400        05 SEQB-ADGANG       PIC S9(3)           COMP-3.                  
002500*                                 G≈NG                                    
002600*                                 AISLE                                   
002700        05 SEQB-ADPLATS      PIC S9(5)           COMP-3.                  
002800*                                 LAGERPLATSNUMMER                        
002900*                                 LOCATION                                
003000     03 SEQB-IDPRTINV.                                                    
003100*                                 PRINTNINGSIDENTITET                     
003200*                                 PRINTIDENTITY                           
003300        05 SEQB-IDPRTOMG     PIC S9              COMP-3.                  
003400*                                 PRINT OMG≈NG F÷R AUT.JUSTERING          
003500*                                 PRINT ROUND OF AUT.ADJUSTMENT           
003600        05 SEQB-IDLOPNR      PIC S9(5)           COMP-3.                  
003700*                                 L÷PNUMMER          IDLOPNR-002          
003800     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 SEQB-KDINVKAT        PIC S9(3)           COMP-3.                  
004200*                                 INVENTERINGSKATEGORI                    
004300*                                 STOCKTAKING CATEGORY                    
004400*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
