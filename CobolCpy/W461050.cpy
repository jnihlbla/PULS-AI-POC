000100 01  ART-W461050.                                                         
000200*                                 ARTIKELINFORMATION TILL NOAC            
000300*                                 PT-050                                  
000400     03 ART-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 ART-REKSIFFR         PIC S9              COMP-3.                  
000900*                                 KONTROLLSIFFRA                          
001000     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200     03 ART-KDSRA            PIC S9(3)           COMP-3.                  
001300*                                 SRA-KOD                                 
001400     03 ART-KVQPACK-1        PIC S9(5)           COMP-3.                  
001500*                                 ANTAL I Q1 FÖRPACKNING                  
001600     03 ART-KDARTURS         PIC X(2).                                    
001700*                                 ARTIKELURSPRUNGSKOD                     
001800     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
001900*                                 PRODUKTSLAG                             
002000     03 ART-VLARTNTO         PIC S9(8)V9(1)      COMP-3.                  
002100*                                 ARTIKELVOLYM NETTO (CM3)                
002200     03 ART-VKART            PIC S9(7)           COMP-3.                  
002300*                                 ARTIKELVIKT (G)                         
002400     03 ART-KDVSOP           PIC S9(3)           COMP-3.                  
002500*                                 VSOP-KOD                                
002600     03 ART-PRARTBTO-EXP     PIC S9(7)V9(2)      COMP-3.                  
002700*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
002800     03 ART-IDSTATNR         OCCURS 6 TIMES                               
002900                             PIC S9(9)           COMP-3.                  
003000*                                 STATISTISKT NUMMER                      
003100*                                 1 = NORSKT                              
003200*                                 2 = ENGELSKT                            
003300*                                 3 = BELGISKT                            
003400*                                 4 = PERUANSKT                           
003500*                                 5 = SVENSKT                             
003600*                                 6 =                                     
003700     03 ART-KDUART           PIC X.                                       
003800*                                 UNDANTAGSARTIKEL                        
003900     03 ART-KDSORT           PIC X(2).                                    
004000*                                 SORT-KOD                                
004100     03 ART-KDERS            PIC S9(3)           COMP-3.                  
004200*                                 ERSÄTTNINGSKOD                          
004300     03 ART-KDBPSR           PIC S9              COMP-3.                  
004400*                                 BASLAGERFÖRSLAGSNIVÅ                    
004500     03 ART-IDLEVNR          PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700     03 ART-KDBBCL           PIC 9.                                       
004800*                                 RETURNERBAR ARTIKEL                     
004900     03 ART-KDARTRAB-A       PIC 9(2).                                    
005000*                                 RABATTKOD (ARTIKELPRIS)                 
005100     03 ART-PRARTBTO-MARK-A  PIC S9(7)V9(2)      COMP-3.                  
005200*                                 BRUTTOPRIS PER MARKNAD (FOB)            
005300     03 ART-KDARTRAB-B       PIC 9(2).                                    
005400*                                 RABATTKOD (ARTIKELPRIS)                 
005500     03 ART-PRARTBTO-MARK-B  PIC S9(7)V9(2)      COMP-3.                  
005600*                                 BRUTTOPRIS PER MARKNAD (FOB)            
005700     03 ART-KDARTRAB-C       PIC 9(2).                                    
005800*                                 RABATTKOD (ARTIKELPRIS)                 
005900     03 ART-PRARTBTO-MARK-C  PIC S9(7)V9(2)      COMP-3.                  
006000*                                 BRUTTOPRIS PER MARKNAD (FOB)            
006100     03 ART-KDARTRAB-D       PIC 9(2).                                    
006200*                                 RABATTKOD (ARTIKELPRIS)                 
006300     03 ART-PRARTBTO-MARK-D  PIC S9(7)V9(2)      COMP-3.                  
006400*                                 BRUTTOPRIS PER MARKNAD (FOB)            
006500     03 ART-KDARTRAB-E       PIC 9(2).                                    
006600*                                 RABATTKOD (ARTIKELPRIS)                 
006700     03 ART-PRARTBTO-MARK-E  PIC S9(7)V9(2)      COMP-3.                  
006800*                                 BRUTTOPRIS PER MARKNAD (FOB)            
006900     03 ART-KDARTRAB-F       PIC 9(2).                                    
007000*                                 RABATTKOD (ARTIKELPRIS)                 
007100     03 ART-PRARTBTO-MARK-F  PIC S9(7)V9(2)      COMP-3.                  
007200*                                 BRUTTOPRIS PER MARKNAD (FOB)            
007300     03 ART-KDARTRAB-G       PIC 9(2).                                    
007400*                                 RABATTKOD (ARTIKELPRIS)                 
007500     03 ART-PRARTBTO-MARK-G  PIC S9(7)V9(2)      COMP-3.                  
007600*                                 BRUTTOPRIS PER MARKNAD (FOB)            
007700     03 ART-KDAGE            PIC X.                                       
007800*                                 AGE-CODE                                
007900*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
