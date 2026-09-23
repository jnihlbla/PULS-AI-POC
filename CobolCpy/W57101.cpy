000100 01  ACS-W57101.                                                          
000200*                                 URVALSDATA TILL ACS                     
000300     03 ACS-IDDC             PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 ACS-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 ACS-ADART.                                                        
001000*                                 ARTIKELADRESS I LAGRET                  
001100*                                 PARTS-ADRESS                            
001200        05 ACS-ADLAGOMR      PIC S9(3)           COMP-3.                  
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500        05 ACS-ADGANG        PIC S9(3)           COMP-3.                  
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800        05 ACS-ADPLATS       PIC S9(5)           COMP-3.                  
001900*                                 LAGERPLATSNUMMER                        
002000*                                 LOCATION                                
002100     03 ACS-BEART            PIC X(25).                                   
002200*                                 ARTIKELBENÄMNING                        
002300*                                 PART DESCRIPTION                        
002400     03 ACS-IDUSER-PCOUNT    PIC X(8).                                    
002500*                                 ANVÄNDARENS SÄKERHETS ID                
002600*                                 USER SECURITY-IDENTITY                  
002700     03 ACS-IDUSER-PCOUNT-REG                                             
002800                             PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000*                                 USER SECURITY-IDENTITY                  
003100     03 ACS-IDUSER-RCOUNT    PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300*                                 USER SECURITY-IDENTITY                  
003400     03 ACS-IDUSER-RCOUNT-REG                                             
003500                             PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800     03 ACS-KDPRODSL         PIC S9(3)           COMP-3.                  
003900*                                 PRODUKTSLAG                             
004000*                                 PRODUCT GROUP                           
004100     03 ACS-KDPSLLOC         PIC 9(2).                                    
004200*                                 PRODUKTSLAG LOKALT                      
004300*                                 PRODUCT GROUP LOCAL                     
004400     03 ACS-KVLS             PIC S9(7)           COMP-3.                  
004500*                                 LAGERSALDO                              
004600*                                 STOCK BALANCE                           
004700     03 ACS-KVPCOUNT         PIC S9(7)           COMP-3.                  
004800*                                 LAGERSALDO                              
004900*                                 STOCK BALANCE                           
005000     03 ACS-KVRCOUNT         PIC S9(7)           COMP-3.                  
005100*                                 LAGERSALDO                              
005200*                                 STOCK BALANCE                           
005300     03 ACS-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
005400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005500*                                 AVERAGE COST FOREIGN CURRENCY           
005600     03 ACS-TIAVCOST         PIC S9(7)           COMP-3.                  
005700*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
005800*                                 AVERAGE COST CALCULATION DATE           
005900     03 ACS-TIINVDAT         PIC S9(5)           COMP-3.                  
006000*                                 INVENTERINGSDATUM                       
006100*                                 STOCKTAKING DATE                        
006200     03 ACS-TIORDREG         PIC S9(7)           COMP-3.                  
006300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
006400*                                 ORDER REGISTRATION DATE  YYMMDD         
006500     03 ACS-TIREGDAT-PCOUNT  PIC S9(7)           COMP-3.                  
006600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006700*                                 REGISTRATION DATE (YYMMDD)              
006800     03 ACS-TIREGDAT-PCOUNT-REG                                           
006900                             PIC S9(7)           COMP-3.                  
007000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007100*                                 REGISTRATION DATE (YYMMDD)              
007200     03 ACS-TIREGDAT-RCOUNT  PIC S9(7)           COMP-3.                  
007300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007400*                                 REGISTRATION DATE (YYMMDD)              
007500     03 ACS-TIREGDAT-RCOUNT-REG                                           
007600                             PIC S9(7)           COMP-3.                  
007700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007800*                                 REGISTRATION DATE (YYMMDD)              
007900     03 ACS-TIREGTID-PCOUNT  PIC S9(7)           COMP-3.                  
008000*                                 REGISTRERINGSTID                        
008100*                                 GENERAL REGISTRATION TIME               
008200     03 ACS-TIREGTID-PCOUNT-REG                                           
008300                             PIC S9(7)           COMP-3.                  
008400*                                 REGISTRERINGSTID                        
008500*                                 GENERAL REGISTRATION TIME               
008600     03 ACS-TIREGTID-RCOUNT  PIC S9(7)           COMP-3.                  
008700*                                 REGISTRERINGSTID                        
008800*                                 GENERAL REGISTRATION TIME               
008900     03 ACS-TIREGTID-RCOUNT-REG                                           
009000                             PIC S9(7)           COMP-3.                  
009100*                                 REGISTRERINGSTID                        
009200*                                 GENERAL REGISTRATION TIME               
009300     03 ACS-TIRETUR-BEORD    PIC S9(7)           COMP-3.                  
009400*                                 DATUM RETUR BEORDRING                   
009500*                                 DATE ISSUE OF RETURN ORDER              
009600     03 ACS-TISKROT-BEORD    PIC S9(7)           COMP-3.                  
009700*                                 BEORDRAD SKROTNINGSDATUM                
009800*                                 DATE OF SCRAPPING DECISION              
009900*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
