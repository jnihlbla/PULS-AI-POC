000100 01  ONORD-W461020.                                                       
000200*                                 ON ORDER TRANSAKTION FÖR ORDER          
000300*                                 FRÅN VR SYSTEMET PT 020                 
000400     03 ONORD-IDPTYP         PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 ONORD-IDDISTR        PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 ONORD-IDKUNDNR       PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 ONORD-IDORDNR        PIC S9(7)           COMP-3.                  
001100*                                 ORDERNR             IDORDNR-002         
001200     03 ONORD-IDDC           PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 ONORD-KDORDKL        PIC S9              COMP-3.                  
001500*                                 ORDERKLASS                              
001600     03 ONORD-BEVARREF       PIC X(10).                                   
001700*                                 VÅR REFERENS                            
001800     03 ONORD-BEVOLREF       PIC X(10).                                   
001900*                                 VOLVO REFERENS                          
002000     03 ONORD-BERADREF       PIC X(10).                                   
002100*                                 KUNDENS RADREFERENS                     
002200     03 ONORD-IDARTNR        PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400     03 ONORD-REKSIFFR       PIC S9              COMP-3.                  
002500*                                 KONTROLLSIFFRA                          
002600     03 ONORD-KVBEART        PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800     03 ONORD-TIORDREG       PIC S9(7)           COMP-3.                  
002900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003000     03 ONORD-IDRONR         PIC S9(5)           COMP-3.                  
003100*                                 RESTORDERNUMMER                         
003200     03 ONORD-TIRODAT        PIC S9(7)           COMP-3.                  
003300*                                 RESTORDERDATUM         (ÅÅMMDD)         
003400     03 ONORD-PRARTNTO       PIC S9(7)V9(2)      COMP-3.                  
003500*                                 ARTIKELPRIS NETTO                       
003600     03 ONORD-PRARTBTO-EXP   PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
003800     03 ONORD-KDPRODSL       PIC S9(3)           COMP-3.                  
003900*                                 PRODUKTSLAG                             
004000     03 ONORD-IDFKNGRP       PIC S9(5)           COMP-3.                  
004100*                                 FUNKTIONSGRUPP                          
004200     03 ONORD-FLINVEST       PIC X.                                       
004300*                                 BYTES INVENTERINGSFLAGGA                
004400     03 ONORD-KDDSP          PIC S9              COMP-3.                  
004500*                                 PÅVERKAN PÅ DSP                         
004600     03 ONORD-KDMANPR        PIC X.                                       
004700*                                 MANUELLT PRIS ELLER PRISTILLÄGG         
004800     03 ONORD-FLABON         PIC X.                                       
004900*                                 ABBONEMANGSDLAGGA                       
005000     03 ONORD-KDTPOTYP       PIC S9              COMP-3.                  
005100*                                 TYP AV TIDPLANERAD ORDER                
005200     03 FILLER               PIC X(3).                                    
005300*** END COPY W461020     LENGTH=91                                        
