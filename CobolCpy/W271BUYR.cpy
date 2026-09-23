000100 01  BUYR-W271BUYR.                                                       
000200*                                 RETURNS REFILL BUYER AND TABLE          
000300     03 BUYR-IN-UTDATA.                                                   
000400        05 BUYR-INDATA.                                                   
000500           07 BUYR-KDCALL    PIC S9(3)           COMP-3.                  
000600*                                 ANROPSTYP                               
000700*                                 CALL TYPE                               
000800           07 BUYR-IDARTNR   PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100           07 BUYR-IDPERSON-BUY-IN                                        
001200                             PIC S9(3)           COMP-3.                  
001300*                                 PERSONKOD REFILLANSVARIG                
001400*                                 REFILL RESPONSIBLE ID                   
001500           07 BUYR-FLBUYUPD  PIC X.                                       
001600*                                 OM IDPERSONKOD ÄR LÅST                  
001700*                                 IF BUYER UPDATE IS LOCKED               
001800           07 BUYR-IDREFTAB-IN                                            
001900                             PIC X.                                       
002000*                                 IDENTITET REFILLTABELL                  
002100*                                 REFILLINGTABLE IDENTIFIER               
002200           07 BUYR-FLTABUPD  PIC X.                                       
002300*                                 OM REFILLTABELL ÄR LÅST                 
002400*                                 IF REFILLINGTABLE UPDATE LOCKED         
002500           07 BUYR-KDPRODSL  PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700*                                 PRODUCT GROUP                           
002800           07 BUYR-IDFKNGRP  PIC S9(5)           COMP-3.                  
002900*                                 FUNKTIONSGRUPP                          
003000*                                 FUNCTION GROUP                          
003100           07 BUYR-KDUART    PIC X.                                       
003200*                                 UNDANTAGSARTIKEL                        
003300*                                 EXECPTION PARTS                         
003400           07 BUYR-IDDC      PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700           07 BUYR-IDDC-REF  PIC X(2).                                    
003800*                                 SÄNDANDE LAGER FÖR REFILL               
003900*                                 SENDING WAREHOUSE FOR REFILL            
004000           07 BUYR-FLFLYG    PIC X.                                       
004100*                                 FLYGARTIKEL                             
004200*                                 PART NUMBER SENT BY AIR                 
004300           07 BUYR-KDFARLIG  PIC S9              COMP-3.                  
004400*                                 KOD FÖR FARLIGT GODS                    
004500*                                 DANGEROUS GOODS CODE                    
004600           07 BUYR-VLARTNTO  PIC S9(8)V9(1)      COMP-3.                  
004700*                                 ARTIKELVOLYM (CM3)                      
004800*                                 PART VOLUME    (CM3)                    
004900           07 BUYR-KVPB-TOT  PIC S9(6)V9(1)      COMP-3.                  
005000*                                 TOTALT PERIODBEHOV                      
005100           07 BUYR-TISOP     PIC S9(5)           COMP-3.                  
005200*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
005300*                                 START OF PRODUCTION,(YYWWD D=1)         
005400           07 BUYR-TIURPROD  PIC S9(5)           COMP-3.                  
005500*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
005600*                                 OUT OF PRODUCTION DATE (YYWW)           
005700           07 BUYR-TISOP-2-YEARS-AGO                                      
005800                             PIC S9(5)           COMP-3.                  
005900*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
006000*                                 START OF PRODUCTION,(YYWWD D=1)         
006100           07 BUYR-TISOP-TODAY                                            
006200                             PIC S9(5)           COMP-3.                  
006300*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
006400*                                 START OF PRODUCTION,(YYWWD D=1)         
006500           07 BUYR-TIURPROD-5-YEARS-AGO                                   
006600                             PIC S9(5)           COMP-3.                  
006700*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006800*                                 OUT OF PRODUCTION DATE (YYWW)           
006900           07 BUYR-FLBSNES   PIC X.                                       
007000*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
007100*                                 PARTS WITH MORE BUSINESS VALUE          
007200           07 BUYR-KVEOP     PIC 9(2).                                    
007300*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
007400*                                 YEARS TO KEEP PART AFTER EOP            
007500        05 BUYR-UTDATA.                                                   
007600           07 BUYR-KDSVAR    PIC X.                                       
007700            88 BUYR-KDSVAR-OK                                             
007800                             VALUE ' '.                                   
007900            88 BUYR-KDSVAR-FEL                                            
008000                             VALUE 'F'.                                   
008100*                                                       KDSVAR-88         
008200*                                 SVARSKOD FRÅN SUBPROGRAM                
008300*                                                       KDSVAR-88         
008400*                                 RETURN CODE FROM SUBPROGRAM             
008500           07 BUYR-IDPERSON-BUY                                           
008600                             PIC S9(3)           COMP-3.                  
008700*                                 PERSONKOD REFILLANSVARIG                
008800*                                 REFILL RESPONSIBLE ID                   
008900           07 BUYR-IDREFTAB  PIC X.                                       
009000*                                 IDENTITET REFILLTABELL                  
009100*                                 REFILLINGTABLE IDENTIFIER               
009200           07 BUYR-FLBUYER-CHANGED                                        
009300                             PIC X.                                       
009400*                                 ALLMÄN FLAGGA                           
009500*                                 GENERAL FLAG                            
009600           07 BUYR-FLTABLE-CHANGED                                        
009700                             PIC X.                                       
009800*                                 ALLMÄN FLAGGA                           
009900*                                 GENERAL FLAG                            
010000           07 BUYR-TEXT      PIC X(25).                                   
010100*** END OF VILMAII-COPY LENGTH= 82 BYTES                                  
