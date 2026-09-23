000100 01  3-W221L803.                                                          
000200*                                 LÄNKAREA NR 3                           
000300*                                 LÄSNING AV WLARTC01                     
000400     03 3-IDARTNR            PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 3-FLERS              PIC X.                                       
000800*                                 TILLKOMMANDE ARTIKEL ?                  
000900     03 3-FLIART             PIC X.                                       
001000*                                 ARTIKELN INGÅR I SATS                   
001100*                                 PART IN KIT                             
001200     03 3-IDAO               OCCURS 5 TIMES                               
001300                             PIC X(10).                                   
001400*                                 ÄNDRINGSORDERNUMMER                     
001500*                                 DESIGN CHANGE NOTICE                    
001600     03 3-IDFKNGRP           PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800*                                 FUNCTION GROUP                          
001900     03 3-IDFTG              PIC 9(2).                                    
002000*                                 FÖRETAGSID EKONOM REDOVISNING           
002100*                                 COMPANY IDENTITY ACCOUNTING             
002200     03 3-IDLEVNR            PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 3-KDERS-UTG          PIC S9(3)           COMP-3.                  
002600*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
002700*                                 OBSOLETION SUPERSESSION CODE            
002800     03 3-KDPRODSL           PIC S9(3)           COMP-3.                  
002900*                                 PRODUKTSLAG                             
003000*                                 PRODUCT GROUP                           
003100     03 3-KDSORT             PIC X(2).                                    
003200*                                 SORT-KOD                                
003300*                                 UNIT OF MEASURE                         
003400     03 3-REKSIFFR           PIC S9              COMP-3.                  
003500*                                 KONTROLLSIFFRA                          
003600*                                 PART NO CHECK DIGIT                     
003700     03 3-TIERSDAT           PIC S9(5)           COMP-3.                  
003800*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003900*                                 DATE OF SUPERSESSION (YYWWD)            
004000     03 3-TIFINLV            PIC S9(5)           COMP-3.                  
004100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004200*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004300     03 3-TIREGDAT           PIC S9(7)           COMP-3.                  
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500*                                 REGISTRATION DATE (YYMMDD)              
004600*** END OF VILMAII-COPY LENGTH= 84 BYTES                                  
