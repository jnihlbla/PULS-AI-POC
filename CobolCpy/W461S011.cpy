000100 01  FREF-W461S011-CTX.                                                   
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 FAKTURA-REFERNS INFO TILL NOAC          
000400     03 FREF-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 FREF-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 FREF-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 FREF-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 FREF-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 FREF-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 FREF-W461011-CTX.                                                 
001700*                                 FAKTURA-REFERENS                        
001800*                                 TILL NOAC PT-011                        
001900        05 FREF-IDPTYP       PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 FREF-IDDISTR      PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 FREF-IDKUNDNR     PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 FREF-KDFAKTYP     PIC X.                                       
002600*                                 FAKTURATYP                              
002700        05 FREF-IDFAKT       PIC S9(7)           COMP-3.                  
002800*                                 FAKTURANUMMER                           
002900        05 FREF-KDSORT2      PIC S9(3)           COMP-3.                  
003000*                                 SORTERINGSFÄLT                          
003100        05 FREF-IDPRODNR     PIC S9(7)           COMP-3.                  
003200*                                 PRODUKTIONSNUMMER                       
003300        05 FREF-IDKOLLI      PIC S9(5)           COMP-3.                  
003400*                                 KOLLINUMMER                             
003500        05 FREF-IDARTNR      PIC S9(9)           COMP-3.                  
003600*                                 ARTIKELNUMMER                           
003700        05 FREF-IDORDNR      PIC S9(7)           COMP-3.                  
003800*                                 ORDERNR             IDORDNR-002         
003900        05 FREF-TIORDREG     PIC S9(7)           COMP-3.                  
004000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004100        05 FREF-KDORDKL      PIC S9              COMP-3.                  
004200*                                 ORDERKLASS                              
004300        05 FREF-BEKUNDRF     PIC X(15).                                   
004400*                                 KUNDENS REFERENS                        
004500        05 FREF-BEVOLREF     PIC X(10).                                   
004600*                                 VOLVO REFERENS                          
004700        05 FREF-BEVARREF     PIC X(10).                                   
004800*                                 VÅR REFERENS                            
004900        05 FREF-KDREFNOT     PIC X(2).                                    
005000*                                 FAKTURA NOTERINGAR                      
005100        05 FREF-KDFRAKT      PIC S9(3)           COMP-3.                  
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300        05 FREF-IDTRPBO.                                                  
005400*                                 BOLLA-DOKUMENT IDENTITET                
005500           07 FREF-IDTRPBOT  PIC X.                                       
005600*                                 BOLLA-DOKUMENT TECKEN                   
005700           07 FREF-IDTRPBON  PIC S9(7)           COMP-3.                  
005800*                                 BOLLA-DOKUMENT NUMMER                   
005900        05 FREF-VKORDBTO-ORDER                                            
006000                             PIC S9(6)V9(1)      COMP-3.                  
006100*                                 ORDERVIKT BRUTTO PER ORDER              
006200*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
