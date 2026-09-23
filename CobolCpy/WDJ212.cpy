000100 01  URAD-WDJ212.                                                         
000200*                                 SATSORDERREGISTER                       
000300*                                 SATSORDERRADER UTSKRIFT SEGMENT         
000400*                                 FYSISK NYCKEL: WDJ212KY                 
000500*                                 (KDSATLI,  ADLAGOMR, ADGANG  ,          
000600*                                  ADPLATS,  IDARTNR)                     
000700     03 URAD-KDSATLI         PIC X.                                       
000800*                                 LISTTYP SATSORDER                       
000900*                                 KIND OF REPORT  KIT-ORDER               
001000     03 URAD-ADLAGOMR        PIC S9(3)           COMP-3.                  
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 URAD-ADGANG          PIC S9(3)           COMP-3.                  
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600     03 URAD-ADPLATS         PIC S9(5)           COMP-3.                  
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900     03 URAD-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 URAD-BEART           PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400*                                 PART DESCRIPTION                        
002500     03 URAD-FLSATBRI        PIC X.                                       
002600*                                 FLAGGA BRISTMARKERING SATS              
002700*                                 FLAG SHORTAGE DANGER KIT                
002800     03 URAD-IDANALYS        PIC X(12).                                   
002900*                                 ANALYSNUMMER                            
003000*                                 ANALYSIS NUMBER                         
003100     03 URAD-IDKONTO         PIC S9(11)          COMP-3.                  
003200*                                 KONTO                                   
003300*                                 ACCOUNT                                 
003400     03 URAD-IDKST           PIC X(10).                                   
003500*                                 KOSTNADSSTÄLLE                          
003600*                                 COST CENTRE                             
003700     03 URAD-KDPRODSL        PIC S9(3)           COMP-3.                  
003800*                                 PRODUKTSLAG                             
003900*                                 PRODUCT GROUP                           
004000     03 URAD-KDSATAND        PIC X.                                       
004100*                                 ÄNDRINGSKOD SATS                        
004200*                                 CHANGE-CODE KIT                         
004300     03 URAD-KDSATKMB        PIC X.                                       
004400*                                 KOMBINATIONSKOD SATS                    
004500*                                 MATCHING CODE KIT                       
004600     03 URAD-KDSORT          PIC X(2).                                    
004700*                                 SORT-KOD                                
004800*                                 UNIT OF MEASURE                         
004900     03 URAD-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
005000*                                 ARTIKELSTANDARDPRIS                     
005100*                                 STANDARD PRICE                          
005200     03 URAD-REANTPSA        PIC S9(2)V9(3)      COMP-3.                  
005300*                                 ANTAL PER SATS                          
005400     03 URAD-REBEART         PIC S9(7)           COMP-3.                  
005500*                                 ANTAL PER ORDERRAD SATS                 
005600*                                 NUMBER PER ORDER-LINE KIT               
005700     03 URAD-REKSIFFR        PIC S9              COMP-3.                  
005800*                                 KONTROLLSIFFRA                          
005900*                                 PART NO CHECK DIGIT                     
006000     03 URAD-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
006100*                                 ARTIKELVIKT NETTO (KG)                  
006200*                                 PART NET WEIGHT (KG)                    
006300     03 URAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
006400*                                 ARTIKELVOLYM NETTO (CM3)                
006500*                                 PART NET VOLUME    (CM3)                
006600*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
