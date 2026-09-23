000100 01  BYPASS-W461S015.                                                     
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 OKÄND ORDER INFO TILL NOAC              
000400     03 BYPASS-SOR0-IDDISTR  PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 BYPASS-SOR0-IDKUNDNR PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 BYPASS-SOR0-IDRONR   PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 BYPASS-SOR0-TIRODAT  PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 BYPASS-SOR0-IDPTYP   PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 BYPASS-SOR0-IDLOPNR  PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 BYPASS-W461015.                                                   
001700*                                 ORDER TRANS OKÄND I VIPS                
001800*                                 TILL NOAC PT 015                        
001900        05 BYPASS-IDPTYP     PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 BYPASS-IDDISTR    PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 BYPASS-IDKUNDNR   PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 BYPASS-IDORDNR    PIC S9(7)           COMP-3.                  
002600*                                 ORDERNR             IDORDNR-002         
002700        05 BYPASS-KDORDKL    PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900        05 BYPASS-IDARTNR    PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100        05 BYPASS-REKSIFFR   PIC S9              COMP-3.                  
003200*                                 KONTROLLSIFFRA                          
003300        05 BYPASS-BERADREF   PIC X(10).                                   
003400*                                 KUNDENS RADREFERENS                     
003500        05 BYPASS-BEVOLREF   PIC X(10).                                   
003600*                                 VOLVO REFERENS                          
003700        05 BYPASS-KVBEART    PIC S9(7)           COMP-3.                  
003800*                                 BESTÄLLT ANTAL ARTIKLAR                 
003900        05 BYPASS-KDFAKTYP   PIC X.                                       
004000*                                 FAKTURATYP                              
004100        05 BYPASS-KDDSP      PIC S9              COMP-3.                  
004200*                                 PÅVERKAN PÅ DSP                         
004300        05 BYPASS-FLABON     PIC X.                                       
004400*                                 ABBONEMANGSDLAGGA                       
004500        05 BYPASS-KDTPOTYP   PIC S9              COMP-3.                  
004600*                                 TYP AV TIDPLANERAD ORDER                
004700        05 FILLER            PIC X(4).                                    
004800*** END COPY W461S015    LENGTH=74                                        
