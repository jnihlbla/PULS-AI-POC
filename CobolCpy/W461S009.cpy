000100 01  SERV-W461S009.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 SERVICEGRADSINFO TILL NOAC              
000400     03 SERV-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 SERV-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 SERV-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 SERV-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 SERV-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 SERV-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 SERV-W461009.                                                     
001700*                                 SERVICE GRAD TILL NOAC PT-009           
001800        05 SERV-IDPTYP       PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 SERV-IDDC         PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 SERV-IDDISTR      PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 SERV-IDKUNDNR     PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 SERV-IDORDNR      PIC S9(7)           COMP-3.                  
002700*                                 ORDERNR             IDORDNR-002         
002800        05 SERV-KDORDKL      PIC S9              COMP-3.                  
002900*                                 ORDERKLASS                              
003000        05 SERV-IDARTNR      PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200        05 SERV-REKSIFFR     PIC S9              COMP-3.                  
003300*                                 KONTROLLSIFFRA                          
003400        05 SERV-KDPRODSL     PIC S9(3)           COMP-3.                  
003500*                                 PRODUKTSLAG                             
003600        05 SERV-KVBEART      PIC S9(7)           COMP-3.                  
003700*                                 BESTÄLLT ANTAL STYCKEN                  
003800        05 SERV-TIORDREG     PIC S9(7)           COMP-3.                  
003900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004000        05 SERV-KDFAKTYP     PIC X.                                       
004100*                                 FAKTURATYP                              
004200        05 FILLER            PIC X(6).                                    
004300*** END COPY W461S009    LENGTH=61                                        
