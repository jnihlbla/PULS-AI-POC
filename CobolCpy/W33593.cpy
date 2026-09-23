000100 01  W33593.                                                              
000200*                                 ARTIKELFIL MED NY SJÄLVKOST PER         
000300*                                  MÅNAD                                  
000400*                                 FRAMSTÄLLS DAGLIGEN OCH VECKOSL         
000500*                                 UT                                      
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR-HUV          PIC X(5).                                    
000900*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
001000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELSTANDARDPRIS                     
001400     03 PRHANTK              PIC S9(5)V9(2)      COMP-3.                  
001500*                                 DIR LÖN  + DIR MTRL + ÖVR PÅL           
001600*                                 PRDIRLON + PRDMTRL  + PROVRPAL          
001700*                                                                         
001800*                                                                         
001900*                                 DIR LÖN AVSER KALKYLERADE OMKOS         
002000*                                 TNADER I KR PER ST AV EN AR-            
002100*                                 TIKEL FÖR FÖRPACKNINGSVERKSAMHE         
002200*                                 T I SAMBAND MED INLEVERANS.             
002300*                                 SORT: KR                                
002400*                                                                         
002500*                                 DIR MTRL AVSER KALKYLERADE KOST         
002600*                                 NADER I KR PER ST AV EN AR-             
002700*                                 TIKEL FÖR FÖRPACKNINGSMATERIAL          
002800*                                 I SAMBAND MED INLEVERANS.               
002900*                                 RANS.                                   
003000*                                 UPPDATERAS PÅ LB ENDAST VIA KT          
003100*                                 24.                                     
003200*                                 SORT: KR                                
003300*                                                                         
003400*                                 OVR PAL AVSER KALKYLERADE OMKOS         
003500*                                 TNADER I KR PER ENHET FÖR IN-           
003600*                                  DIREKTA ÖVRIGA OMKOSTNADSPÅLÄG         
003700*                                 G AVSER KALKYLERADE   KOST-             
003800*                                  NADER FÖR ÖVR DIREKTA LÖNER OC         
003900*                                 H FÖRBRUKNINGSMATERIAL I RS             
004000*                                  FÖRPACKNINGSVERKSAMHET VID INL         
004100*                                 EVERANS.                                
004200*                                 UPPDATERAS PÅ LB ENDAST VIA TT          
004300*                                 R24 PRISÄNDRING.                        
004400*                                 SORT: KR                                
004500     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004600*                                 ARTIKELNS SJÄLVKOSTNAD                  
004700     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 BESTÄLLNINGSPRIS I KRONOR               
004900     03 KVAL                 PIC S9              COMP-3.                  
005000*                                 ANTAL LEVERANTÖRER                      
005100     03 FILLER               OCCURS 9 TIMES.                              
005200        05 IDLEVNR           PIC X(5).                                    
005300*                                 LEVERANTÖRNUMMER                        
005400        05 IDLEVNR-MOTSV     PIC X(5).                                    
005500*                                 MOTSVARANDE LEVERANTÖRSID               
005600        05 PRARTBES-PR       PIC S9(7)V9(2)      COMP-3.                  
005700*                                 DETTA BESTÄLLNINGSPRIS (KR)             
005800*** END OF VILMAII-COPY LENGTH= 167 BYTES                                 
