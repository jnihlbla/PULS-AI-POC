000100 01  OBKVAN-W461006.                                                      
000200*                                 ORDERBEKR.  KVANTANPASSNING             
000300*                                 TILL NOAC PT-006                        
000400     03 OBKVAN-IDPTYP        PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBKVAN-IDDISTR       PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBKVAN-IDKUNDNR      PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBKVAN-KDFRAKT       PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBKVAN-IDORDNR       PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBKVAN-KDLIDEL       PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBKVAN-IDDC          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBKVAN-IDARTNR       PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBKVAN-REKSIFFR      PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBKVAN-BEART         PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBKVAN-BERADREF      PIC X(10).                                   
002500*                                 KUNDENS RADREFERENS                     
002600     03 OBKVAN-IDRONR        PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERNUMMER      IDRONR-002         
002800     03 OBKVAN-TIRODAT       PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERDATUM         (ÅÅMMDD)         
003000     03 OBKVAN-BEVOLREF      PIC X(10).                                   
003100*                                 VOLVO REFERENS                          
003200     03 OBKVAN-KDRESTR       PIC S9(3)           COMP-3.                  
003300*                                 RESTRIKTIONSKOD                         
003400     03 OBKVAN-KVBEART       PIC S9(7)           COMP-3.                  
003500*                                 BESTÄLLT ANTAL STYCKEN                  
003600     03 OBKVAN-KVBEART-Q     PIC S9(7)           COMP-3.                  
003700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003800     03 OBKVAN-KVQPACK-1     PIC S9(5)           COMP-3.                  
003900*                                 ANTAL I Q1 FÖRPACKNING                  
004000     03 OBKVAN-KDKVBRYT      PIC S9              COMP-3.                  
004100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004200     03 OBKVAN-KDDSP         PIC S9              COMP-3.                  
004300*                                 PÅVERKAN PÅ DSP                         
004400     03 OBKVAN-KDFAKTYP      PIC X.                                       
004500*                                 FAKTURATYP                              
004600     03 OBKVAN-TIAAMMDD      PIC S9(7)           COMP-3.                  
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800     03 OBKVAN-TIKLOCK       PIC S9(9)           COMP-3.                  
004900*                                 KLOCKSLAG (TTMMSSTH)                    
005000*** END COPY W461006     LENGTH=103                                       
