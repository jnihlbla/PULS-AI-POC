000100 01  ONORD-W461S020.                                                      
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ON ORDER POST TILL NOAC                 
000400     03 ONORD-SOR0-IDDISTR   PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 ONORD-SOR0-IDKUNDNR  PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 ONORD-SOR0-IDRONR    PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 ONORD-SOR0-TIRODAT   PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 ONORD-SOR0-IDPTYP    PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 ONORD-SOR0-IDLOPNR   PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 ONORD-W461020.                                                    
001700*                                 ON ORDER TRANSAKTION FÖR ORDER          
001800*                                 FRÅN VR SYSTEMET PT 020                 
001900        05 ONORD-IDPTYP      PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 ONORD-IDDISTR     PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 ONORD-IDKUNDNR    PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 ONORD-IDORDNR     PIC S9(7)           COMP-3.                  
002600*                                 ORDERNR             IDORDNR-002         
002700        05 ONORD-IDDC        PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 ONORD-KDORDKL     PIC S9              COMP-3.                  
003000*                                 ORDERKLASS                              
003100        05 ONORD-BEVARREF    PIC X(10).                                   
003200*                                 VÅR REFERENS                            
003300        05 ONORD-BEVOLREF    PIC X(10).                                   
003400*                                 VOLVO REFERENS                          
003500        05 ONORD-BERADREF    PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700        05 ONORD-IDARTNR     PIC S9(9)           COMP-3.                  
003800*                                 ARTIKELNUMMER                           
003900        05 ONORD-REKSIFFR    PIC S9              COMP-3.                  
004000*                                 KONTROLLSIFFRA                          
004100        05 ONORD-KVBEART     PIC S9(7)           COMP-3.                  
004200*                                 BESTÄLLT ANTAL STYCKEN                  
004300        05 ONORD-TIORDREG    PIC S9(7)           COMP-3.                  
004400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004500        05 ONORD-IDRONR      PIC S9(5)           COMP-3.                  
004600*                                 RESTORDERNUMMER                         
004700        05 ONORD-TIRODAT     PIC S9(7)           COMP-3.                  
004800*                                 RESTORDERDATUM         (ÅÅMMDD)         
004900        05 ONORD-PRARTNTO    PIC S9(7)V9(2)      COMP-3.                  
005000*                                 ARTIKELPRIS NETTO                       
005100        05 ONORD-PRARTBTO-EXP                                             
005200                             PIC S9(7)V9(2)      COMP-3.                  
005300*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
005400        05 ONORD-KDPRODSL    PIC S9(3)           COMP-3.                  
005500*                                 PRODUKTSLAG                             
005600        05 ONORD-IDFKNGRP    PIC S9(5)           COMP-3.                  
005700*                                 FUNKTIONSGRUPP                          
005800        05 ONORD-FLINVEST    PIC X.                                       
005900*                                 BYTES INVENTERINGSFLAGGA                
006000        05 ONORD-KDDSP       PIC S9              COMP-3.                  
006100*                                 PÅVERKAN PÅ DSP                         
006200        05 ONORD-KDMANPR     PIC X.                                       
006300*                                 MANUELLT PRIS ELLER PRISTILLÄGG         
006400        05 ONORD-FLABON      PIC X.                                       
006500*                                 ABBONEMANGSDLAGGA                       
006600        05 ONORD-KDTPOTYP    PIC S9              COMP-3.                  
006700*                                 TYP AV TIDPLANERAD ORDER                
006800        05 FILLER            PIC X(3).                                    
006900*** END COPY W461S020    LENGTH=112                                       
