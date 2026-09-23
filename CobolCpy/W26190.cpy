000100 01  W26190.                                                              
000200*                                 ARTIKLAR MED TIURPROD                   
000300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000400*                                 LAGEROMRÅDE                             
000500     03 ADPLATS              PIC S9(5)           COMP-3.                  
000600*                                 LAGERPLATSNUMMER                        
000700     03 BEART                PIC X(25).                                   
000800*                                 ARTIKELBENÄMNING                        
000900     03 FLERS                PIC X.                                       
001000*                                 TILLKOMMANDE ARTIKEL ?                  
001100     03 FLFKNPRI             PIC X.                                       
001200*                                 FLAGGA=FKNGRP VIKTIG                    
001300     03 FLIART               PIC X.                                       
001400*                                 ARTIKELN INGÅR I SATS                   
001500     03 FLSKROT-BEORD        PIC X.                                       
001600*                                 SKROTNING BEORDRAD AV ANSK              
001700     03 IDANSK               PIC S9(3)           COMP-3.                  
001800*                                 ANSKAFFARNUMMER                         
001900     03 IDARTNR              PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 IDDC                 PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002400*                                 FUNKTIONSGRUPP                          
002500     03 IDLEVNR              PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 KDERS                PIC S9(3)           COMP-3.                  
002800*                                 ERSÄTTNINGSKOD                          
002900     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
003000*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003200*                                 PRODUKTSLAG                             
003300     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
003400*                                 DEL AV AK PÅ VÄG                        
003500     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
003600*                                 DEL AV AK SOM LIGGER I SDC              
003700     03 KVLS                 PIC S9(7)           COMP-3.                  
003800*                                 LAGERSALDO                              
003900     03 KVOKS-BULK           PIC S9(7)           COMP-3.                  
004000*                                 ORDERKÖSALDO, KLASS 2-4                 
004100     03 KVOKS-DAG            PIC S9(7)           COMP-3.                  
004200*                                 ORDERKÖSALDO, KLASS 1                   
004300     03 KVRESS               PIC S9(7)           COMP-3.                  
004400*                                 RESERVERAT ANTAL ARTIKLAR               
004500     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
004600*                                 SLUTKÖPSSALDO                           
004700     03 KVSPANT              PIC S9(7)           COMP-3.                  
004800*                                 SPÄRRAT ANTAL                           
004900     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
005000*                                 FAST PRIS UNDER LÖPANDE ÅR              
005100     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
005200*                                 DIREKTLEVERANSANDEL                     
005300     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
005400*                                 SUMMA LEVERERAT ANTAL                   
005500*                                 AV 1 ARTIKEL                            
005600     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
005700*                                 SUMMA BRUTTOVINSTVÄRDE                  
005800     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
005900*                                 SUMMA BRUTTOVINSTVÄRDE                  
006000     03 TISKROT-AUTO         PIC S9(7)           COMP-3.                  
006100*                                 STOPDATE AUTO-SKROTNING                 
006200     03 TISKROT-BEORD        PIC S9(7)           COMP-3.                  
006300*                                 BEORDRAD SKROTNINGSDATUM                
006400     03 TISLUTKP             PIC S9(7)           COMP-3.                  
006500*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
006600     03 TIURPROD             PIC S9(5)           COMP-3.                  
006700*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006800*** END OF VILMAII-COPY LENGTH= 130 BYTES                                 
