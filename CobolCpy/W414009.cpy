000100 01  AMC-W414009.                                                         
000200*                                 SKAPAS FÖR ORDERRAD VID                 
000300*                                 TÖMNING AV TRANSAKTIONER.               
000400*                                 GÄLLER PRISFEL                          
000500*                                 SÄNDS TILL VIOS.                        
000600     03 AMC-IDPTYP           PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 AMC-IDMARKBO         PIC X.                                       
000900*                                 MARKNADSBOLAGSKOD                       
001000     03 AMC-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 AMC-IDKUNDNR         PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 AMC-IDORDER          PIC S9(7)           COMP-3.                  
001500*                                 VOLVO PARTS ORDERNUMMER                 
001600     03 AMC-KDORDKL          PIC S9              COMP-3.                  
001700*                                 ORDERKLASS                              
001800     03 AMC-KDFAKTYP         PIC X.                                       
001900*                                 FAKTURATYP                              
002000     03 AMC-IDARTNR          PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 AMC-KDPRODSL         PIC S9(3)           COMP-3.                  
002300*                                 PRODUKTSLAG                             
002400     03 AMC-IDFKNGRP         PIC S9(5)           COMP-3.                  
002500*                                 FUNKTIONSGRUPP                          
002600     03 AMC-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
002700*                                 ARTIKELPRIS NETTO                       
002800     03 AMC-KVBEART          PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000     03 AMC-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
003100*                                 ARTIKELNS SJÄLVKOSTNAD                  
003200     03 AMC-TIREGDAT         PIC S9(7)           COMP-3.                  
003300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003400     03 AMC-TIRODAT          PIC S9(7)           COMP-3.                  
003500*                                 RESTORDERDATUM         (ÅÅMMDD)         
003600     03 AMC-PRBPRIS          PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BASPRIS                                 
003800     03 AMC-IDFELKOD         PIC X(3).                                    
003900*                                 FELKOD                                  
004000*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
