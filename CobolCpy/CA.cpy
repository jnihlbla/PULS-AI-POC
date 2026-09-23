000100* GENERATION OF COBOL HOST STRUCTURE FROM CA-TAB                          
000200  01 CA.                                                                  
000300*              ALL REPORTED COMPANYS PARTS                                
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDARTNR20                         PIC X(20).                        
000700*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
000800*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
000900*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001000   03 BEART                             PIC X(25).                        
001100*              ARTIKELBENÄMNING                                           
001200   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
001300*              LEVERANTÖR ENLIGT AVTAL                                    
001400   03 IDLEVNR                           PIC S9(5) COMP-3.                 
001500*              LEVERANTÖRNUMMER                                           
001600   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001700*              PRODUKTSLAG                                                
001800   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
001900*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
002000   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
002100*              FUNKTIONSGRUPP                                             
002200   03 KDSTAKL                           PIC X(2).                         
002300*              STANDARD KLASS                                             
002400   03 KDAGE-IMP                         PIC X(3).                         
002500*              AGE-CODE IMP-SYSTEM                                        
002600   03 KDABC                             PIC X(1).                         
002700*              ABC KOD                                                    
002800   03 BEEMBLEM-PRIO                     PIC X(5).                         
002900*              PRIORITERAT EMBLEM                                         
003000   03 IDPROJ                            PIC X(4).                         
003100*              PARTS PROJEKTIDENTITET                                     
003200   03 TELOCBUC                          PIC X(5).                         
003300*              LOCAL BUCKET                                               
003400   03 KDARTKAM                          PIC S9(5) COMP-3.                 
003500*              TRANSFER KOD                                               
003600   03 KDSACOND                          PIC S9(5) COMP-3.                 
003700*              SALES CONDITION NUMBER                                     
003800   03 KDSADISC                          PIC S9(3) COMP-3.                 
003900*              SALES CONDITION CODE                                       
004000   03 KDRABATT                          PIC S9(3) COMP-3.                 
004100*              RABATTKOD                                                  
004200   03 IDCOMP                            PIC S9(5) COMP-3.                 
004300*              THE NUMBER ASSIGED TO A CERTAIN                            
004400*              COMPETITOR TO BE ABLE TO IDENTIFY HIM                      
004500*              USED IN SALES AND TARGET                                   
004600   03 KDFREE                            PIC X(3).                         
004700*              FREE CODE                                                  
004800   03 IDSTATNR                          PIC S9(9) COMP-3.                 
004900*              STATISTISKT NUMMER                                         
005000*              1 = NORSKT                                                 
005100*              2 = ENGELSKT                                               
005200*              3 = BELGISKT                                               
005300*              4 = PERUANSKT                                              
005400*              5 = SVENSKT                                                
005500*              6 =                                                        
005600   03 KDOBSOL                           PIC X(1).                         
005700*              OBSOLESCENCE CODE                                          
005800   03 KDREPL                            PIC X(1).                         
005900*              REPLACED CODE                                              
006000   03 DASTDUPD                          PIC S9(9) COMP-3.                 
006100*              STANDARD COST DATE                                         
006200   03 IDFAMILY                          PIC S9(7) COMP-3.                 
006300*              FAMILY NUMBER                                              
006400   03 KDERS                             PIC S9(3) COMP-3.                 
006500*              ERSÄTTNINGSKOD                                             
006600   03 RETOTFSG-STOCK                    PIC S9(3)V9(1) COMP-3.            
006700*              MÅNADSORDER I FÖRH TILL TOTFSG                             
006800   03 DAREGDAT                          PIC X(8).                         
006900*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
007000   03 KDSORT                            PIC X(2).                         
007100*              SORT-KOD                                                   
007200*                                                                         
007300*** END OF VILMAII-COPY LENGTH= 127 OLD LENGTH=                           
