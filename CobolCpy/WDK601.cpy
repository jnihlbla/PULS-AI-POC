000100 01  ART-WDK601.                                                          
000200*                                 ARTIKELINFORMATION                      
000300*                                 FYSISK NYCKEL IDARTNR                   
000400*                                 SÖKBEGREPP    KDERS (-UTG)              
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ART-FLERS            PIC X.                                       
000900*                                 TILLKOMMANDE ARTIKEL ?                  
001000     03 ART-FLIART           PIC X.                                       
001100*                                 ARTIKELN INGÅR I SATS                   
001200*                                 PART IN KIT                             
001300     03 ART-IDAO             OCCURS 5 TIMES                               
001400                             PIC X(10).                                   
001500*                                 ÄNDRINGSORDERNUMMER                     
001600*                                 DESIGN CHANGE NOTICE                    
001700     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP                          
001900*                                 FUNCTION GROUP                          
002000     03 ART-IDFTG            PIC 9(2).                                    
002100*                                 FÖRETAGSID EKONOM REDOVISNING           
002200*                                 COMPANY IDENTITY ACCOUNTING             
002300     03 ART-IDLEVNR          PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600     03 ART-KDERS-UTG        PIC S9(3)           COMP-3.                  
002700*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
002800*                                 OBSOLETION SUPERSESSION CODE            
002900     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100*                                 PRODUCT GROUP                           
003200     03 ART-KDSORT           PIC X(2).                                    
003300*                                 SORT-KOD                                
003400*                                 UNIT OF MEASURE                         
003500     03 ART-REKSIFFR         PIC S9              COMP-3.                  
003600*                                 KONTROLLSIFFRA                          
003700*                                 PART NO CHECK DIGIT                     
003800     03 ART-TIERSDAT         PIC S9(5)           COMP-3.                  
003900*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
004000*                                 DATE OF SUPERSESSION (YYWWD)            
004100     03 ART-TIFINLV          PIC S9(5)           COMP-3.                  
004200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004300*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004400     03 ART-TIREGDAT         PIC S9(7)           COMP-3.                  
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600*                                 REGISTRATION DATE (YYMMDD)              
004700     03 ART-TIURPROD         PIC S9(5)           COMP-3.                  
004800*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
004900*                                 OUT OF PRODUCTION DATE (YYWW)           
005000     03 ART-TISOP            PIC S9(5)           COMP-3.                  
005100*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
005200*                                 START OF PRODUCTION,(YYWWD D=1)         
005300     03 ART-FLBRAND          PIC X.                                       
005400*                                 ARTIKEL MED VARUMÄRKESBILD              
005500*                                 PARTS WITH THE BRAND IMAGE              
005600     03 ART-FLBSNES          PIC X.                                       
005700*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
005800*                                 PARTS WITH MORE BUSINESS VALUE          
005900     03 ART-KDSOP            PIC X.                                       
006000*                                 VISAR NÄR START DAT ART GÄLLER          
006100*                                 START OF PARTS CAN BE APPLIED           
006200     03 ART-KVEOP            PIC 9(2).                                    
006300*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
006400*                                 YEARS TO KEEP PART AFTER EOP            
006500     03 ART-KDANSKSEG        PIC S9(5)           COMP-3.                  
006600*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
006700*                                 CODE FOR PROCUREMENT SEGMENT            
006800     03 ART-IDCDS            PIC X(8).                                    
006900*                                 ANVÄNDARENS CDS ID                      
007000*                                 USER CDS SECURITY-IDENTITY              
007100     03 ART-KDARTSYS         PIC X(2).                                    
007200*                                 KOD FÖR SYST. ÄGARE AV ARTIKEL          
007300*                                 COD FOR OWNER SYSTEM OF A PART          
007400     03 ART-FILLERX2         PIC X(2).                                    
007500*** END OF VILMAII-COPY LENGTH= 110 BYTES                                 
