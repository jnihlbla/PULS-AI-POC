000100 01  W3358D-CTX.                                                          
000200*                                 ARTIKELFIL MED NY SJÄLVKOST PER         
000300*                                  MÅNAD                                  
000400*                                 FRAMSTÄLLS DAGLIGEN OCH VECKOSL         
000500*                                 UT                                      
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 IDLEVNR-HUV          PIC X(5).                                    
001100*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
001200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTSLAG                             
001400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001500*                                 ARTIKELSTANDARDPRIS                     
001600     03 PRHANTK              PIC S9(5)V9(2)      COMP-3.                  
001700*                                 DIR LÖN  + DIR MTRL + ÖVR PÅL           
001800*                                 PRDIRLON + PRDMTRL  + PROVRPAL          
001900*                                                                         
002000*                                                                         
002100*                                 DIR LÖN AVSER KALKYLERADE OMKOS         
002200*                                 TNADER I KR PER ST AV EN AR-            
002300*                                 TIKEL FÖR FÖRPACKNINGSVERKSAMHE         
002400*                                 T I SAMBAND MED INLEVERANS.             
002500*                                 SORT: KR                                
002600*                                                                         
002700*                                 DIR MTRL AVSER KALKYLERADE KOST         
002800*                                 NADER I KR PER ST AV EN AR-             
002900*                                 TIKEL FÖR FÖRPACKNINGSMATERIAL          
003000*                                 I SAMBAND MED INLEVERANS.               
003100*                                 RANS.                                   
003200*                                 UPPDATERAS PÅ LB ENDAST VIA KT          
003300*                                 24.                                     
003400*                                 SORT: KR                                
003500*                                                                         
003600*                                 OVR PAL AVSER KALKYLERADE OMKOS         
003700*                                 TNADER I KR PER ENHET FÖR IN-           
003800*                                  DIREKTA ÖVRIGA OMKOSTNADSPÅLÄG         
003900*                                 G AVSER KALKYLERADE   KOST-             
004000*                                  NADER FÖR ÖVR DIREKTA LÖNER OC         
004100*                                 H FÖRBRUKNINGSMATERIAL I RS             
004200*                                  FÖRPACKNINGSVERKSAMHET VID INL         
004300*                                 EVERANS.                                
004400*                                 UPPDATERAS PÅ LB ENDAST VIA TT          
004500*                                 R24 PRISÄNDRING.                        
004600*                                 SORT: KR                                
004700     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELNS SJÄLVKOSTNAD                  
004900     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
005000*                                 BESTÄLLNINGSPRIS I KRONOR               
005100     03 KVAL                 PIC S9              COMP-3.                  
005200*                                 ANTAL LEVERANTÖRER                      
005300     03 IDLEVNR              PIC X(5).                                    
005400*                                 LEVERANTÖRNUMMER                        
005500     03 IDLEVNR-MOTSV        PIC X(5).                                    
005600*                                 MOTSVARANDE LEVERANTÖRSID               
005700     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
005800*                                 DETTA BESTÄLLNINGSPRIS (KR)             
005900*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
