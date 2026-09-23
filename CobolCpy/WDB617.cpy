000100 01  PROC-WDB617.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                 PRISPÅVERKANDE FAKTORER FÖR DC          
000400*                                 I LÄNDER MED EGEN ANSKAFFNING           
000500*                                 FYSISK NYCKEL: KDSEGKEY                 
000600     03 PROC-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 PROC-REDIRLON        PIC S9(3)V9(2)      COMP-3.                  
001000*                                 DIREKT LÖN PROCENT-PÅSLAG               
001100*                                 PERCENT SURCHARGE COSTS WAGES           
001200     03 PROC-REDMTRL         PIC S9(3)V9(2)      COMP-3.                  
001300*                                 DIREKT MATERIAL PROCENT-PÅSLAG          
001400*                                 PERCENT ADDED COST PACKING MTRL         
001500     03 PROC-RELANDCO-EXP    PIC S9(3)V9(3)      COMP-3.                  
001600*                                 LANDING COST PROCENT EXPORTRANS         
001700*                                 LANDING COST PERCENT EXPORT             
001800     03 PROC-RELANDCO-FROM   PIC S9(3)V9(3)      COMP-3.                  
001900*                                 LANDING COST PROCENT FOM                
002000*                                 LANDING COST PERCENT AFTER DATE         
002100     03 PROC-RELANDCO-TO     PIC S9(3)V9(3)      COMP-3.                  
002200*                                 LANDING COST PROCENT TOM                
002300*                                 LANDING COST PERCENT UNTIL DATE         
002400     03 PROC-IDUSER          PIC X(8).                                    
002500*                                 ANVÄNDARENS SÄKERHETS ID                
002600*                                 USER SECURITY-IDENTITY                  
002700     03 PROC-TIUPPDAT        PIC S9(7)           COMP-3.                  
002800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002900*                                 UPDATING DATE     (YYMMDD)              
003000     03 PROC-TILANDCO        PIC S9(7)           COMP-3.                  
003100*                                 STARTDATUM LANDING COST FAKTOR          
003200*                                 START DATE LANDING COST FACTOR          
003300     03 PROC-RELANDCO-ITX-FROM                                            
003400                             PIC S9(3)V9(3)      COMP-3.                  
003500*                                 LANDING COST/FG IT EFTER TITULF         
003600*                                 LANDING COST/FG IT AFTER TITULF         
003700     03 PROC-RELANDCO-ITX-TO PIC S9(3)V9(3)      COMP-3.                  
003800*                                 LANDING COST/FG IT TILL TITULF          
003900*                                 LANDING COST/FG IT UNTIL TITULF         
004000     03 PROC-RELANDCO-GTX-FROM                                            
004100                             PIC S9(3)V9(3)      COMP-3.                  
004200*                                 LANDING COST/FG GT EFTER TITULF         
004300*                                 LANDING COST/FG GT AFTER TITULF         
004400     03 PROC-RELANDCO-GTX-TO PIC S9(3)V9(3)      COMP-3.                  
004500*                                 LANDING COST/FG GT TILL TITULF          
004600*                                 LANDING COST/FG GT UNTIL TITULF         
004700     03 PROC-RELANDCO-OCF-FROM                                            
004800                             PIC S9(3)V9(3)      COMP-3.                  
004900*                                 LANDING COST/FG OC EFTER TITULF         
005000*                                 LANDING COST/FG OC AFTER TITULF         
005100     03 PROC-RELANDCO-OCF-TO PIC S9(3)V9(3)      COMP-3.                  
005200*                                 LANDING COST/FG OC TILL TITULF          
005300*                                 LANDING COST/FG OC UNTIL TITULF         
005400*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
