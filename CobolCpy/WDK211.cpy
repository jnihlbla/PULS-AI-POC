000100 01  MATD-WDK211.                                                         
000200*                                 ARTIKEL-MÄTDATA                         
000300*                                 INLÄSTA MASKINELLT                      
000400*                                 FYSISK NYCKEL KDSEGKEY                  
000500     03 MATD-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 MATD-KDVSOP          PIC S9(3)           COMP-3.                  
000900*                                 VSOP-KOD                                
001000*                                 VSOP-CODE                               
001100     03 MATD-KVHEIGHT-BTO    PIC S9(4)V9(1)      COMP-3.                  
001200*                                 ARTIKELNS BRUTTOHÖJD                    
001300*                                 GROSS HEIGHT OF PART                    
001400     03 MATD-KVHEIGHT-NTO    PIC S9(4)V9(1)      COMP-3.                  
001500*                                 ARTIKELNS NETTOHÖJD                     
001600*                                 NET HEIGHT OF PART                      
001700     03 MATD-KVLENGTH-BTO    PIC S9(4)V9(1)      COMP-3.                  
001800*                                 ARTIKELNS BRUTTOLÄNGD                   
001900*                                 GROSS LENGTH OF PART                    
002000     03 MATD-KVLENGTH-NTO    PIC S9(4)V9(1)      COMP-3.                  
002100*                                 ARTIKELNS NETTOLÄNGD                    
002200*                                 NET LENGTH OF PART                      
002300     03 MATD-KVWIDTH-BTO     PIC S9(4)V9(1)      COMP-3.                  
002400*                                 ARTIKELNS BRUTTOBREDD                   
002500*                                 GROSS WIDTH OF PART                     
002600     03 MATD-KVWIDTH-NTO     PIC S9(4)V9(1)      COMP-3.                  
002700*                                 ARTIKELNS NETTOBREDD                    
002800*                                 NET WIDTH OF PART                       
002900     03 MATD-TIUPPDAT        PIC S9(7)           COMP-3.                  
003000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003100*                                 UPDATING DATE     (YYMMDD)              
003200     03 MATD-VKART-BTO       PIC S9(9)           COMP-3.                  
003300*                                 ART BRUTTOVIKT MASKINELLT               
003400*                                 PART GROSS WEIGHT FROM MACHINE          
003500     03 MATD-VKART-NTO       PIC S9(9)           COMP-3.                  
003600*                                 ARTIKELNS NETTOVIKT                     
003700*                                 PART NET WEIGHT                         
003800     03 MATD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
003900*                                 ARTIKELVOLYM (CM3)                      
004000*                                 PART VOLUME    (CM3)                    
004100*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
