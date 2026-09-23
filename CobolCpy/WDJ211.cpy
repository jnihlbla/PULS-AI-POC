000100 01  SRAD-WDJ211.                                                         
000200*                                 SATSORDERREGISTER                       
000300*                                 SATSORDERRADER SEGMENT                  
000400*                                 FYSISK NYCKEL: IDARTNR                  
000500     03 SRAD-IDARTNR         PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 SRAD-FLSATRAS        PIC X.                                       
000900*                                 FLAGGA FLER FINNS I RASA  SATS          
001000*                                 FLAG MORE EXIST IN RASA   KIT           
001100     03 SRAD-FLSATUTS        PIC X.                                       
001200*                                 ORDERRADER UTSKRIVNA I SATS             
001300*                                 ORDER-LINES WRITTEN IN KIT-ORDE         
001400*                                 RS                                      
001500     03 SRAD-IDANALYS        PIC X(12).                                   
001600*                                 ANALYSNUMMER                            
001700*                                 ANALYSIS NUMBER                         
001800     03 SRAD-IDKONTO         PIC S9(11)          COMP-3.                  
001900*                                 KONTO                                   
002000*                                 ACCOUNT                                 
002100     03 SRAD-IDKST           PIC X(10).                                   
002200*                                 KOSTNADSSTÄLLE                          
002300*                                 COST CENTRE                             
002400     03 SRAD-KDCLAGER        PIC S9              COMP-3.                  
002500*                                 CENTRALLAGERKOD                         
002600*                                 CENTRAL WAREHOUSE CODE                  
002700     03 SRAD-KDPRODSL        PIC S9(3)           COMP-3.                  
002800*                                 PRODUKTSLAG                             
002900*                                 PRODUCT GROUP                           
003000     03 SRAD-KDSATAND        PIC X.                                       
003100*                                 ÄNDRINGSKOD SATS                        
003200*                                 CHANGE-CODE KIT                         
003300     03 SRAD-KDSATKMB        PIC X.                                       
003400*                                 KOMBINATIONSKOD SATS                    
003500*                                 MATCHING CODE KIT                       
003600     03 SRAD-KDSORT          PIC X(2).                                    
003700*                                 SORT-KOD                                
003800*                                 UNIT OF MEASURE                         
003900     03 SRAD-KDSTRRAD        PIC X.                                       
004000*                                 TYP AV STRUKTURRAD                      
004100*                                 TYPE OF LINE IN A STRUCTURE             
004200     03 SRAD-KVSATRES        PIC S9(7)           COMP-3.                  
004300*                                 RESERVERAT ANTAL ARTIKLAR SATS          
004400*                                 RESERVED QTY PER ORDER-LINE KIT         
004500     03 SRAD-KVSATROS        PIC S9(7)           COMP-3.                  
004600*                                 RESTNOTERAT ANTAL ARTIKLAR SATS         
004700*                                 BACKORDERED QTY ORDER-LINE KIT          
004800     03 SRAD-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
004900*                                 ARTIKELSTANDARDPRIS                     
005000*                                 STANDARD PRICE                          
005100     03 SRAD-REANTPSA        PIC S9(2)V9(3)      COMP-3.                  
005200*                                 ANTAL PER SATS                          
005300     03 SRAD-REBEART         PIC S9(7)           COMP-3.                  
005400*                                 ANTAL PER ORDERRAD SATS                 
005500*                                 NUMBER PER ORDER-LINE KIT               
005600     03 SRAD-REKSIFFR        PIC S9              COMP-3.                  
005700*                                 KONTROLLSIFFRA                          
005800*                                 PART NO CHECK DIGIT                     
005900     03 SRAD-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
006000*                                 ARTIKELVIKT NETTO (KG)                  
006100*                                 PART NET WEIGHT (KG)                    
006200     03 SRAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
006300*                                 ARTIKELVOLYM NETTO (CM3)                
006400*                                 PART NET VOLUME    (CM3)                
006500     03 SRAD-FLSATSPR        PIC X.                                       
006600*                                 FLAGGA SPÄRRAD SATS EL SATSRAD          
006700*                                 FLAG ORDER OR LINE BLOCKED KIT          
006800*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
