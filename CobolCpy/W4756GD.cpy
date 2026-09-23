000100 01  BROKJPD-W4756GD.                                                     
000200*                                 BROKER INFORMATION JAPAN                
000300*                                 DETAIL                                  
000400     03 BROKJPD-IDPTYP       PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 BROKJPD-IDFAKT       PIC S9(7)           COMP-3.                  
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 BROKJPD-IDORDNR7     PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200*                                 ORDER NUMBER                            
001300     03 BROKJPD-IDARTNR      PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 BROKJPD-IDKOLLI      PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800*                                 CASE NUMBER                             
001900     03 BROKJPD-KVLEVART     PIC S9(7)           COMP-3.                  
002000*                                 LEVERERAT ANTAL STYCK                   
002100*                                 DELIVERED QUANTITY                      
002200     03 BROKJPD-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
002300*                                 ARTIKELPRIS NETTO                       
002400*                                 NET PRICE EACH   (FOB NET)              
002500     03 BROKJPD-VKARTNTO     PIC S9(4)V9(3)      COMP-3.                  
002600*                                 ARTIKELVIKT NETTO (KG)                  
002700*                                 PART NET WEIGHT (KG)                    
002800     03 BROKJPD-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
002900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
003000*                                 GROSS-PRICE EXPORT                      
003100*                                  (FOB-GROSS)                            
003200     03 BROKJPD-KDARTURS     PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400*                                 COUNTRY OF ORIGIN                       
003500     03 BROKJPD-IDSTATNR     PIC S9(9)           COMP-3.                  
003600*                                 STATISTISKT NUMMER                      
003700*                                 1 = NORSKT                              
003800*                                 2 = ENGELSKT                            
003900*                                 3 = BELGISKT                            
004000*                                 4 = PERUANSKT                           
004100*                                 5 = SVENSKT                             
004200*                                 6 =                                     
004300*                                 STATISTICAL NO.                         
004400     03 BROKJPD-KDSORT       PIC X(2).                                    
004500*                                 SORT-KOD                                
004600*                                 UNIT OF MEASURE                         
004700     03 BROKJPD-VKLEV        PIC S9(4)V9(3)      COMP-3.                  
004800*                                 ARTIKELVIKT NETTO (KG)                  
004900*                                 PART NET WEIGHT (KG)                    
005000     03 BROKJPD-BEART        PIC X(25).                                   
005100*                                 ARTIKELBENÄMNING                        
005200*                                 PART DESCRIPTION                        
005300*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
