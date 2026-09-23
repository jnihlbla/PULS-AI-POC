000100 01  W51094X.                                                             
000200*                                 POSTTYP 94X FAKTURARAD                  
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 KDFAKTYP             PIC X.                                       
000700*                                 FAKTURATYP                              
000800     03 IDFAKT               PIC S9(7)           COMP-3.                  
000900*                                 FAKTURANUMMER                           
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 IDDEALER             PIC S9(7)           COMP-3.                  
001700*                                 DEALER KUNDNUMMER                       
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 FLDIRLEV             PIC X.                                       
002100*                                 DIREKTLEVERANS ?                        
002200     03 KVLEVART             PIC S9(7)           COMP-3.                  
002300*                                 LEVERERAT ANTAL STYCK                   
002400     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
002500*                                 ARTIKELPRIS NETTO                       
002600     03 SUARTNTO             PIC S9(9)V9(2)      COMP-3.                  
002700*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
002800     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
002900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003000     03 IDKONTO              PIC S9(11)          COMP-3.                  
003100*                                 KONTO                                   
003200     03 IDKST                PIC X(10).                                   
003300*                                 KOSTNADSSTÄLLE                          
003400     03 KDORDTYP             PIC S9              COMP-3.                  
003500*                                 ORDERTYP                                
003600     03 IDKUNDRF             PIC X(10).                                   
003700*                                 KUNDENS REFERENS (ORDERID)              
003800     03 BERADREF             PIC X(10).                                   
003900*                                 KUNDENS RADREFERENS                     
004000     03 FLLSBOK              PIC X.                                       
004100*                                 LAGERAVBOKNING                          
004200*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
