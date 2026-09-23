000100 01  BILL-W476FAKT.                                                       
000200*                                 FAKTURATRANSAR FRÅN BILL-IT             
000300     03 BILL-IDPRODNR        PIC S9(7)           COMP-3.                  
000400*                                 PRODUKTIONSNUMMER                       
000500     03 BILL-IDKOLLI         PIC S9(5)           COMP-3.                  
000600*                                 KOLLINUMMER                             
000700     03 BILL-IDPURAD         PIC S9(5)           COMP-3.                  
000800*                                 RADNUMMER PÅ PACKUNDERLAG               
000900     03 BILL-BEART           PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 BILL-DAFINDOC        PIC 9(8).                                    
001200*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001300     03 BILL-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 BILL-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700     03 BILL-IDKUNDNR        PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900     03 BILL-IDORDNR7        PIC S9(7)           COMP-3.                  
002000*                                 ORDERNUMMER                             
002100     03 BILL-IDFAKT          PIC S9(7)           COMP-3.                  
002200*                                 FAKTURANUMMER                           
002300     03 BILL-IDFKNGRP        PIC S9(5)           COMP-3.                  
002400*                                 FUNKTIONSGRUPP                          
002500     03 BILL-IDPARTNR        PIC X(9).                                    
002600*                                 PARTNERNUMMER                           
002700     03 BILL-IDSHIPM         PIC 9(7).                                    
002800*                                 SKEPPNINGSNUMMER                        
002900     03 BILL-KDARTRAB        PIC 9(2).                                    
003000*                                 RABATTKOD (ARTIKELPRIS)                 
003100     03 BILL-KDPRODSL        PIC S9(3)           COMP-3.                  
003200*                                 PRODUKTSLAG                             
003300     03 BILL-KDVALISO        PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500     03 BILL-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
003600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003700     03 BILL-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
003800*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
003900*                                 UTL.VALUTA                              
004000     03 BILL-SUBTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
004100*                                 TOTAL SALES AMOUNT PER LINE INC         
004200*                                 L. VAT                                  
004300     03 BILL-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
004400*                                 TOTAL SALES AMOUNT INCL. VAT            
004500     03 BILL-SUNTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
004600*                                 TOTAL SALES AMOUNT PER LINE EXC         
004700*                                 L. VAT                                  
004800     03 BILL-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
004900*                                 TOTAL SALES AMOUNT EXCL. VAT            
005000     03 BILL-SUVAT-FAKT      PIC S9(11)V9(2)     COMP-3.                  
005100*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
005200*                                 EDITNOTA                                
005300     03 BILL-SUVAT-LINE      PIC S9(11)V9(2)     COMP-3.                  
005400*                                 MOMSVÄRDE PER FAKTURARAD                
005500     03 BILL-TIFINDOC        PIC S9(7)           COMP-3.                  
005600*                                 DOKUMENT KLOCKSLAG (TTMMSS)             
005700     03 BILL-TISKEPPN        PIC S9(7)           COMP-3.                  
005800*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
005900     03 BILL-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
006000*                                 ARTIKELVIKT NETTO (KG)                  
006100     03 BILL-KDLEVVIL        PIC S9              COMP-3.                  
006200*                                 LEVERANSVILLKOR                         
006300*** END OF VILMAII-COPY LENGTH= 151 BYTES                                 
