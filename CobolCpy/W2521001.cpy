000100 01  W2521001.                                                            
000200     03 IDARTNR              PIC S9(9)           COMP-3.                  
000300*                                 ARTIKELNUMMER                           
000400     03 IDPROJ               PIC X(4).                                    
000500*                                 PARTS PROJEKTIDENTITET                  
000600     03 KDPRODSL-URVAL       PIC 9(3)            COMP-3.                  
000700*                                 PRODUKTSLAG        KDPRODSL-002         
000800     03 KDPRODSL-ARTIKEL     PIC 9(3)            COMP-3.                  
000900*                                 PRODUKTSLAG        KDPRODSL-002         
001000     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200     03 IDANSK               PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400     03 KDAVT                PIC S9              COMP-3.                  
001500*                                 AVTALSMÄRKNING                          
001600     03 IDINK                PIC X(4).                                    
001700*                                 INKÖPARNUMMER                           
001800     03 IDLEVNR              PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 KVAKS                PIC S9(7)           COMP-3.                  
002100*                                 ANKOMSTSALDO                            
002200     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
002300*                                 DEL AV AK SOM LIGGER I CDC              
002400     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002500*                                 DEL AV AK PÅ VÄG                        
002600     03 KVAKS-T              PIC S9(7)           COMP-3.                  
002700*                                 DEL AV AK I EN TERMINAL                 
002800     03 KVLS                 PIC S9(7)           COMP-3.                  
002900*                                 LAGERSALDO                              
003000     03 KVRESS               PIC S9(7)           COMP-3.                  
003100*                                 RESERVERAT ANTAL ARTIKLAR               
003200     03 KVBR                 PIC S9(9)           COMP-3.                  
003300*                                 BESTÄLLNINGSREST       KVBR-003         
003400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003500*                                 ARTIKELSTANDARDPRIS                     
003600     03 KDUART               PIC X.                                       
003700*                                 UNDANTAGSARTIKEL                        
003800     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
003900*                                 DIREKTLEVERANSANDEL                     
004000     03 TIAVIDAT-SEN         PIC S9(7)           COMP-3.                  
004100*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
004200     03 VECKA                OCCURS 8 TIMES                               
004300                             PIC S9(5)           COMP-3.                  
004400*                                 ÅR - VECKA  (ÅÅVV)                      
004500     03 ANT                  OCCURS 8 TIMES                               
004600                             PIC S9(9)           COMP-3.                  
004700*                                 ALLMÄNT ANTAL       KVANTAL-006         
004800     03 KVANT                OCCURS 8 TIMES                               
004900                             PIC S9(9)           COMP-3.                  
005000*                                 ALLMÄNT ANTAL       KVANTAL-006         
005100     03 KDERS                PIC S9(3)           COMP-3.                  
005200*                                 ERSÄTTNINGSKOD                          
005300     03 KDBPSR               PIC S9              COMP-3.                  
005400*                                 BASLAGERFÖRSLAGSNIVÅ                    
005500     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
005600*                                 EMBALLAGEKOD 2                          
005700     03 IDAO                 PIC X(10).                                   
005800*                                 ÄNDRINGSORDERNUMMER                     
005900     03 TIAVTAL              PIC S9(7)           COMP-3.                  
006000*                                 AVTALSDATUM  (ÅÅMMDD)                   
006100     03 TIBEST               PIC S9(7)           COMP-3.                  
006200*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006300     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
006400*                                 ANTAL VECKOR LEDTID                     
006500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006600*                                 PRODUKTSLAG                             
006700     03 TIFINLV              PIC S9(5)           COMP-3.                  
006800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006900     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
007000*                                 TPO-KVANTITET, TOTAL                    
007100     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
007200*                                 FUNKTIONSGRUPP                          
007300     03 KVROS                PIC S9(7)           COMP-3.                  
007400*                                 RESTORDERSALDO                          
007500     03 IDPROJUP             PIC X(8).                                    
007600*                                 PROJEKTUPPDRAG                          
007700*** END OF VILMAII-COPY LENGTH= 223 BYTES                                 
