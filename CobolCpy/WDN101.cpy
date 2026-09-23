000100 01  KAT-WDN101.                                                          
000200*                                 KATALOGREGISTER                         
000300*                                 ROTSEGMENT                              
000400*                                 FYSISK NYCKEL: IDCATNR                  
000500     03 KAT-IDCATNR          PIC 9(5).                                    
000600*                                 KATALOG-ID                              
000700*                                 CATALOG-ID                              
000800     03 KAT-BEEMBLEM         PIC X(5).                                    
000900*                                 EMBLEM                                  
001000*                                 EMBLEM                                  
001100     03 KAT-BEMASTER         PIC X(12).                                   
001200*                                 MASTERNAMN FÖR FORDON                   
001300*                                 MASTER NAME FOR VEHICLE                 
001400     03 KAT-FLKATVAD         PIC X.                                       
001500*                                 KATALOG TILL VADIS?                     
001600*                                 CATALOGUE TO VADIS?                     
001700     03 KAT-FLKOPIE          PIC X.                                       
001800*                                 TILLÅTEN ATT LÅNA HÄRIFRÅN              
001900*                                 MAY BE COPIED FROM                      
002000     03 KAT-IDPARTGRP        OCCURS 6 TIMES                               
002100                             PIC X(6).                                    
002200*                                 PARTNER-GRUPP                           
002300*                                 PARTNER GROUP                           
002400     03 KAT-KDFORDON         PIC X(2).                                    
002500*                                 FORDONSSLAG                             
002600*                                 TYPE OF VEHICLE CODE                    
002700     03 KAT-KDCATPUB-FOM     PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, F.O.M.             
002900*                                 RELEASE TIME CODE, FROM                 
003000     03 KAT-TENOTE           PIC X(40).                                   
003100*                                 NOTERINGSFÄLT                           
003200*                                 NOTE FIELD                              
003300     03 KAT-TIHIST           PIC S9(7)           COMP-3.                  
003400*                                 FLYTTNINGSDATUM                         
003500*                                 DATE MOVED TO HISTORICAL REG            
003600     03 KAT-TIOMBRYT         OCCURS 17 TIMES                              
003700                             PIC S9(7)           COMP-3.                  
003800*                                 OMBRYTNINGSDATUM                        
003900*                                 DATE OF PAGE MAKING UP                  
004000     03 KAT-TIOMBRYT-F       PIC S9(7)           COMP-3.                  
004100*                                 FÖRSTA OMBRYTNINGSDATUM                 
004200*                                 FIRST DATE OF PAGE MAKING UP            
004300     03 KAT-TIOMBRYT-ORD     PIC S9(7)           COMP-3.                  
004400*                                 DATUM DÅ OMBRYTNING BEORDRATS           
004500*                                 ORDER DATE OF PAGE MAKING UP            
004600     03 KAT-TIOMBRYT-PUBL    PIC S9(7)           COMP-3.                  
004700*                                 PUBLICERINGSDATUM OMBRYTNING            
004800*                                 RELEASE DATE OF PAGE MAKING UP          
004900     03 KAT-TIOMBRYT-SEN     PIC S9(7)           COMP-3.                  
005000*                                 SENASTE OMBRYTNINGSDATUM                
005100*                                 LAST DATE OF PAGE MAKING UP             
005200     03 KAT-TIREGDAT         PIC S9(7)           COMP-3.                  
005300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005400*                                 REGISTRATION DATE (YYMMDD)              
005500     03 KAT-BECAT.                                                        
005600*                                 KATALOGBETECKNING                       
005700*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
005800*                                 CATALOG DECRIPTION NAME                 
005900*                                 SEE ALSO BEEMBLEM RESP BEMASTER         
006000        05 KAT-BECAT-RAD1    PIC X(40).                                   
006100*                                 KATALOGBETECKNING                       
006200*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
006300*                                 CATALOG DECRIPTION NAME                 
006400*                                 SEE ALSO BEEMBLEM RESP BEMASTER         
006500        05 KAT-BECAT-RAD2    PIC X(20).                                   
006600*                                 KATALOGBETECKNING                       
006700*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
006800*                                 CATALOG DECRIPTION NAME                 
006900*                                 SEE ALSO BEEMBLEM RESP BEMASTER         
007000     03 KAT-MODELL           OCCURS 8 TIMES.                              
007100*                                 MODELL OCH VARIANTBETECKNING            
007200        05 KAT-IDMODELL      PIC X(3).                                    
007300*                                 BILENS NUMERISKA MODELLBET.             
007400*                                 NUMERIC MODEL ID FOR A VECHICLE         
007500        05 KAT-TIMODAAR-STA  PIC 9(4).                                    
007600*                                 MODELLÅR (ÅÅÅÅ) STARTÅR                 
007700*                                 MODEL YEAR (YYYY) START YEAR            
007800        05 KAT-TIMODAAR-STO  PIC 9(4).                                    
007900*                                 MODELLÅR (ÅÅÅÅ) STOPPÅR                 
008000*                                 MODEL YEAR (YYYY) STOP YEAR             
008100        05 KAT-IDVARIANT     PIC X(15).                                   
008200*                                 BILVARIANT                              
008300*                                 VEHICLE VARIANT                         
008400*** END OF VILMAII-COPY LENGTH= 468 BYTES                                 
