000100 01  BOPU1-TAB.                                                           
000200*                                 BUDGET- OCH PROGNOSUPPFÖLJNING          
000300*                                 NIVÅ 1,                                 
000400*                                 VILKET INNEBÄR ATT NYCKELN BL.A         
000500*                                  BESTÅR AV                              
000600*                                 MARKNAD, PRODUKTSLAG OCH ENSKIL         
000700*                                 DA PERIODER.                            
000800     03 TIAA                 PIC S9(3)           COMP-3.                  
000900*                                 ÅR    (ÅÅ)                              
001000     03 KDPLATYP             PIC X(3).                                    
001100*                                 PLANERINGSTYP BUDGET, PROGNOS,          
001200*                                 UTFALL                                  
001300     03 KDFTGNIV             PIC S9              COMP-3.                  
001400*                                 FÖRETAGSNIVÅKOD                         
001500     03 KDPRODKT             PIC S9(3)           COMP-3.                  
001600      88 KDPRODKT-PART-VCC   VALUE +11.                                   
001700      88 KDPRODKT-PART-VTC-VBC                                            
001800                             VALUE +13.                                   
001900      88 KDPRODKT-EMBALL     VALUE +14.                                   
002000      88 KDPRODKT-PART-REN   VALUE +16.                                   
002100      88 KDPRODKT-EXC-VCC    VALUE +21.                                   
002200      88 KDPRODKT-EXC-VTC-VBC                                             
002300                             VALUE +23.                                   
002400      88 KDPRODKT-EXC-REN    VALUE +26.                                   
002500      88 KDPRODKT-ACC-VCC    VALUE +31.                                   
002600      88 KDPRODKT-ACC-VTC-VBC                                             
002700                             VALUE +33.                                   
002800      88 KDPRODKT-RADIO      VALUE +35.                                   
002900      88 KDPRODKT-ACC-REN    VALUE +36.                                   
003000      88 KDPRODKT-TELE       VALUE +38.                                   
003100      88 KDPRODKT-TOOLS      VALUE +41.                                   
003200      88 KDPRODKT-CONSTR     VALUE +42.                                   
003300      88 KDPRODKT-DEX        VALUE +51.                                   
003400      88 KDPRODKT-ADVERT     VALUE +61.                                   
003500*                                 PRODUKTKOD                              
003600     03 KDMARK-BUDG          PIC S9(3)           COMP-3.                  
003700*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003800     03 TIRP                 PIC S9(2)           COMP-3.                  
003900*                                 REDOVISNINGSPERIOD                      
004000*                                 12 PER ÅR                               
004100     03 BEMARK-BUDG          PIC X(15).                                   
004200*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004300     03 SUTOTFSG             PIC S9(11)V9(2)     COMP-3.                  
004400*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
004500     03 SUPKFSG              PIC S9(11)V9(2)     COMP-3.                  
004600*                                 FÖRSÄLJNINGSVÄRDE-PK                    
004700     03 SUGRUFSG             PIC S9(11)V9(2)     COMP-3.                  
004800*                                 FÖRSÄLJNINGSVÄRDE-GRUND                 
004900     03 SUTOTBV              PIC S9(11)V9(2)     COMP-3.                  
005000*                                 SUMMA BRUTTOVINSTVÄRDE                  
005100     03 SUPKBV               PIC S9(11)V9(2)     COMP-3.                  
005200*                                 BRUTTOVINSTVÄRDE-PK                     
005300     03 SUGRUBV              PIC S9(11)V9(2)     COMP-3.                  
005400*                                 BRUTTOVINSTVÄRDE-GRUND                  
005500     03 SUTOTFSG-STD         PIC S9(11)V9(2)     COMP-3.                  
005600*                                 TOTALT FÖRSÄLJNINGSVÄRDE TILL           
005700*                                 STANDARDPRIS                            
005800     03 SUTOTFSG-STDFG       PIC S9(11)V9(2)     COMP-3.                  
005900*                                 TOTALT FÖRSÄLJNINGSVÄRDE TILL           
006000*                                 STANDARDPRIS FÖREGÅENDE ÅR              
006100*** END OF VILMAII-COPY LENGTH= 83 OLD LENGTH= LENGTH=83                  
