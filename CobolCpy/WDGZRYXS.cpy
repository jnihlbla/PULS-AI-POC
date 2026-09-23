000100 01  RYXS-WDGZRYXS-CTX.                                                   
000200*                                 RYX, DEL 2 LÄGGES I SORTAREAN           
000300*                                 SKAPAS VID UTSKRIFT AV                  
000400*                                 EN ORDERRAD BEKR KOD 90 91              
000500*                                 FRÅN W411DEAV.                          
000600*                                 ANVÄNDS FÖR FIL TILL LOGISTIK.          
000700     03 RYXS-FLAKPLOC-UT     PIC X.                                       
000800*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
000900     03 RYXS-KVAVBART-UT     PIC S9(7)           COMP-3.                  
001000*                                 AVBOKAT ANTAL ARTIKLAR                  
001100     03 RYXS-KDORDBEK-UT     PIC 9(2).                                    
001200*                                 ORDERBEKRÄFTELSEKOD                     
001300     03 RYXS-RERF-RAD-UT     PIC S9V9(4)         COMP-3.                  
001400*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
001500     03 RYXS-KVEFRS-UT       PIC S9(7)           COMP-3.                  
001600*                                 EJ FAKTURERAT ANTAL STYCK               
001700     03 RYXS-KVLS-UT         PIC S9(7)           COMP-3.                  
001800*                                 LAGERSALDO                              
001900     03 RYXS-KVRESS-UT       PIC S9(7)           COMP-3.                  
002000*                                 RESERVERAT ANTAL ARTIKLAR               
002100     03 RYXS-KVROS-UT        PIC S9(7)           COMP-3.                  
002200*                                 RESTORDERSALDO                          
002300     03 RYXS-KDROO-UT        PIC S9              COMP-3.                  
002400*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
002500*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
