000100 01  SEGM-W221SEGM.                                                       
000200*                                 RETURNS KDANSKSEG                       
000300     03 SEGM-INDATA.                                                      
000400        05 SEGM-KDCALL       PIC S9(3)           COMP-3.                  
000500*                                 ANROPSTYP                               
000600*                                 CALL TYPE                               
000700        05 SEGM-IDARTNR      PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000        05 SEGM-KDANSKSEG-IN PIC S9(5)           COMP-3.                  
001100*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
001200*                                 CODE FOR PROCUREMENT SEGMENT            
001300        05 SEGM-IDDC         PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600        05 SEGM-IDREFTAB-IN  PIC X.                                       
001700*                                 IDENTITET REFILLTABELL                  
001800*                                 REFILLINGTABLE IDENTIFIER               
001900        05 SEGM-KDPRODSL     PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100*                                 PRODUCT GROUP                           
002200        05 SEGM-FLBSNES      PIC X.                                       
002300*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
002400*                                 PARTS WITH MORE BUSINESS VALUE          
002500        05 SEGM-KVEOP        PIC 9(2).                                    
002600*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
002700*                                 YEARS TO KEEP PART AFTER EOP            
002800        05 SEGM-KDVSOP       PIC S9(3)           COMP-3.                  
002900*                                 VSOP-KOD                                
003000*                                 VSOP-CODE                               
003100        05 SEGM-TISOP        PIC S9(5)           COMP-3.                  
003200*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
003300*                                 START OF PRODUCTION,(YYWWD D=1)         
003400        05 SEGM-TIURPROD     PIC S9(5)           COMP-3.                  
003500*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
003600*                                 OUT OF PRODUCTION DATE (YYWW)           
003700        05 SEGM-TISOP-2-YEARS-AGO                                         
003800                             PIC S9(5)           COMP-3.                  
003900*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
004000*                                 START OF PRODUCTION,(YYWWD D=1)         
004100        05 SEGM-TISOP-TODAY  PIC S9(5)           COMP-3.                  
004200*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
004300*                                 START OF PRODUCTION,(YYWWD D=1)         
004400        05 SEGM-TIURPROD-5-YEARS-AGO                                      
004500                             PIC S9(5)           COMP-3.                  
004600*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
004700*                                 OUT OF PRODUCTION DATE (YYWW)           
004800        05 SEGM-KDFARLIG     PIC S9              COMP-3.                  
004900*                                 KOD FÖR FARLIGT GODS                    
005000*                                 DANGEROUS GOODS CODE                    
005100        05 SEGM-FLTABUPD     PIC X.                                       
005200*                                 OM REFILLTABELL ÄR LÅST                 
005300*                                 IF REFILLINGTABLE UPDATE LOCKED         
005400        05 SEGM-FILLER       PIC X(24).                                   
005500     03 SEGM-UTDATA.                                                      
005600        05 SEGM-KDSVAR       PIC X.                                       
005700         88 SEGM-KDSVAR-OK   VALUE ' '.                                   
005800         88 SEGM-KDSVAR-FEL  VALUE 'F'.                                   
005900*                                                       KDSVAR-88         
006000*                                 SVARSKOD FRÅN SUBPROGRAM                
006100*                                                       KDSVAR-88         
006200*                                 RETURN CODE FROM SUBPROGRAM             
006300        05 SEGM-KDCALL001UTDATA.                                          
006400           07 SEGM-KDANSKSEG PIC S9(5)           COMP-3.                  
006500*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
006600*                                 CODE FOR PROCUREMENT SEGMENT            
006700           07 SEGM-FLANSKSEG-CHANGED                                      
006800                             PIC X.                                       
006900*                                 ALLMÄN FLAGGA                           
007000*                                 GENERAL FLAG                            
007100           07 SEGM-KDFGPRIO  PIC 9(3).                                    
007200*                                 HANTERINGSPRIO FARLIGT GODS             
007300*                                 PRIORITY CODE DANGEROUS GODS            
007400        05 SEGM-KDCALL002UTDATA.                                          
007500           07 SEGM-IDREFTAB  PIC X.                                       
007600*                                 IDENTITET REFILLTABELL                  
007700*                                 REFILLINGTABLE IDENTIFIER               
007800           07 SEGM-FLTABLE-CHANGED                                        
007900                             PIC X.                                       
008000*                                 ALLMÄN FLAGGA                           
008100*                                 GENERAL FLAG                            
008200           07 SEGM-KVOT-RULL12                                            
008300                             PIC S9(7)           COMP-3.                  
008400*                                 ORDERTRÄFFAR PÅ SDC                     
008500*                                 ORDERHITS ON SDC                        
008600           07 SEGM-KVVECKOR-FT                                            
008700                             PIC S9(3)           COMP-3.                  
008800*                                 ANTAL VECKOR FRYSNINGSTID               
008900        05 SEGM-TEXT         PIC X(25).                                   
009000        05 SEGM-FILLER       PIC X(18).                                   
009100*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
