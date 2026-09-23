000100 01  RZA-WDGZRZA.                                                         
000200*                                 POSTTYP RZA                             
000300*                                 SKAPAR ON-ORDERTRANSAR                  
000400*                                 TILL EXTERNA SYSTEM                     
000500     03 RZA-IDPTYP           PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 RZA-BERADREF         PIC X(10).                                   
000800*                                 KUNDENS RADREFERENS                     
000900     03 RZA-BEVARREF         PIC X(10).                                   
001000*                                 VÅR REFERENS                            
001100     03 RZA-BEVOLREF         PIC X(10).                                   
001200*                                 VOLVO REFERENS                          
001300     03 RZA-IDARTNR          PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 RZA-IDFKNGRP         PIC S9(5)           COMP-3.                  
001600*                                 FUNKTIONSGRUPP                          
001700     03 RZA-IDKUNDRF         PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900     03 RZA-IDSYSTEM         PIC X(4).                                    
002000*                                 SKAPANDE SYSTEMNUMMER                   
002100     03 RZA-FLABON           PIC X.                                       
002200*                                 ABBONEMANGSDLAGGA                       
002300     03 RZA-FLINVEST         PIC X.                                       
002400*                                 BYTES INVENTERINGSFLAGGA                
002500     03 RZA-IDDC             PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 RZA-KDDSP            PIC S9              COMP-3.                  
002800*                                 PÅVERKAN PÅ DSP                         
002900     03 RZA-KDFAKTYP         PIC X.                                       
003000*                                 FAKTURATYP                              
003100     03 RZA-KDMANPR          PIC X.                                       
003200*                                 MANUELLT PRIS ELLER PRISTILLÄGG         
003300     03 RZA-KDORDKL          PIC S9              COMP-3.                  
003400*                                 ORDERKLASS                              
003500     03 RZA-KDPRODSL         PIC S9(3)           COMP-3.                  
003600*                                 PRODUKTSLAG                             
003700     03 RZA-KDTPOTYP         PIC S9              COMP-3.                  
003800*                                 TYP AV TIDPLANERAD ORDER                
003900     03 RZA-KDVRINFO         PIC S9              COMP-3.                  
004000*                                 PÅVERKAN I VR/DSP SYSTEM                
004100     03 RZA-KDVRTPO          PIC S9              COMP-3.                  
004200*                                 KOD FÖR TPO:ER FRÅN VR                  
004300     03 RZA-KVBEART          PIC S9(7)           COMP-3.                  
004400*                                 BESTÄLLT ANTAL STYCKEN                  
004500     03 RZA-PRARTBTO-EXP     PIC S9(7)V9(2)      COMP-3.                  
004600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
004700     03 RZA-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELPRIS NETTO                       
004900     03 RZA-REKSIFFR         PIC S9              COMP-3.                  
005000*                                 KONTROLLSIFFRA                          
005100     03 RZA-TIORDREG         PIC S9(7)           COMP-3.                  
005200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005300*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
