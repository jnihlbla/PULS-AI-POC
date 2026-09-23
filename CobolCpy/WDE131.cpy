000100 01  SRAD-WDE131.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 RADINFO                                 
000400*                                 FYS NYCKEL: IDPURAD                     
000500     03 SRAD-IDPURAD         PIC S9(5)           COMP-3.                  
000600*                                 RADNUMMER PÅ PACKUNDERLAG               
000700*                                 LINENO IN PACKINGDOCUMENT               
000800     03 SRAD-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 SRAD-REKSIFFR        PIC S9              COMP-3.                  
001200*                                 KONTROLLSIFFRA                          
001300*                                 PART NO CHECK DIGIT                     
001400     03 SRAD-IDFKNGRP        PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600*                                 FUNCTION GROUP                          
001700     03 SRAD-IDSTATNR        PIC S9(9)           COMP-3.                  
001800*                                 STATISTISKT NUMMER                      
001900*                                 1 = NORSKT                              
002000*                                 2 = ENGELSKT                            
002100*                                 3 = BELGISKT                            
002200*                                 4 = PERUANSKT                           
002300*                                 5 = SVENSKT                             
002400*                                 6 =                                     
002500*                                 STATISTICAL NO.                         
002600     03 SRAD-KDARTURS        PIC X(2).                                    
002700*                                 ARTIKELURSPRUNGSKOD                     
002800*                                 COUNTRY OF ORIGIN                       
002900     03 SRAD-KDPRODSL        PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100*                                 PRODUCT GROUP                           
003200     03 SRAD-KDVALISO        PIC X(3).                                    
003300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003400*                                 CURRENCY CODE BY ISO-STANDARD.          
003500     03 SRAD-KVLEVART        PIC S9(7)           COMP-3.                  
003600*                                 LEVERERAT ANTAL STYCK                   
003700*                                 DELIVERED QUANTITY                      
003800     03 SRAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELPRIS NETTO                       
004000*                                 NET PRICE EACH   (FOB NET)              
004100     03 SRAD-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
004200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004300*                                 NET PRICE EACH LOCAL CURRENCY           
004400     03 SRAD-PRARTNTO-LOCPREL                                             
004500                             PIC S9(7)V9(2)      COMP-3.                  
004600*                                 PREL NETTO SLUTKUNDSPRIS I              
004700*                                 LOKAL VALUTA                            
004800*                                 PREL NET PRICE - LOCAL CURRENCY         
004900     03 SRAD-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
005000*                                 ARTIKELVIKT NETTO (KG) MED EMB          
005100*                                 PART NET WEIGHT (KG) W/ PACKAGE         
005200     03 SRAD-IDKUNDRF-RO     PIC X(10).                                   
005300*                                 KUND REF PÅ RO                          
005400*                                 CUST REF RO                             
005500     03 SRAD-IDLEVNR-ART     PIC X(5).                                    
005600*                                 LEVERANTÖRNR PÅ ARTIKEL                 
005700*                                 PART SUPPLIER NUMBER                    
005800     03 SRAD-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
005900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006000*                                 AVERAGE COST FOREIGN CURRENCY           
006100     03 SRAD-KDVALISO-EXP    PIC X(3).                                    
006200*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
006300*                                 CURRENCY FOR EXPORT (LOC. CURR)         
006400     03 SRAD-VKART-NTO-KG    PIC S9(4)V9(3)      COMP-3.                  
006500*                                 ART. NETTOVIKT I KG UTAN EMB            
006600*                                 PART NET WEIGHT KG NO PACKAGING         
006700*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
