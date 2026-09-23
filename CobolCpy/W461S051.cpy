000100 01  RENS-W461S051.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 RENSNINGS INFO TILL NOAC                
000400     03 RENS-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 RENS-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 RENS-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 RENS-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 RENS-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 RENS-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600     03 RENS-W461051.                                                     
001700*                                 RENSNINGS POST TILL NOAC PT-051         
001800        05 RENS-IDPTYP       PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 RENS-IDDC         PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 RENS-IDDISTR      PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 RENS-IDKUNDNR     PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 RENS-IDORDNR      PIC S9(7)           COMP-3.                  
002700*                                 ORDERNR             IDORDNR-002         
002800        05 RENS-BEVOLREF     PIC X(10).                                   
002900*                                 VOLVO REFERENS                          
003000        05 RENS-TIORDREG     PIC S9(7)           COMP-3.                  
003100*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
003200        05 FILLER            PIC X(6).                                    
003300*** END COPY W461S051    LENGTH=57                                        
