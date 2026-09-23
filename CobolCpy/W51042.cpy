000100 01  W51042.                                                              
000200*                                 HÄNDELSETRANS TILL EKONOMI              
000300     03 IDPGM                PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800*                                 REGISTRATION DATE (YYMMDD)              
000900     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100*                                 TIME OF DAY (HHMMSSTH)                  
001200     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
001300*                                 GENERELLT SEKVENSNUMMER                 
001400*                                 GENERAL SEQUENCE NUMBER                 
001500     03 IDCPYTXT.                                                         
001600*                                 COPYTEXT IDENTITET                      
001700*                                 IDENTITY OF A COPYTEXT                  
001800        05 CT-IDSYSTEM       PIC X(4).                                    
001900*                                 VOLVO VCAS SYSTEMNUMMER                 
002000*                                 VOLVO VCAS SYSTEM NUMBER                
002100        05 CT-IDPTYP         PIC X(3).                                    
002200*                                 POSTTYP                                 
002300*                                 RECORD TYPE                             
002400        05 CT-IDVTYP         PIC X.                                       
002500*                                 POSTTYPSVERSION                         
002600*                                 RECORD TYPE VERSION                     
002700     03 IDPTYP               PIC X(3).                                    
002800*                                 POSTTYP                                 
002900*                                 RECORD TYPE                             
003000     03 IDDC-SEND            PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200*                                 WAREHOUSE IDENTIFIER                    
003300     03 IDDC-REC             PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500*                                 WAREHOUSE IDENTIFIER                    
003600     03 KDEKOHT              PIC X(3).                                    
003700*                                 KOD EKONOMISK HÄNDELSE                  
003800*                                 CODE ECONOMIC EVENT                     
003900     03 KDFAKTYP             PIC X.                                       
004000*                                 FAKTURATYP                              
004100*                                 INVOICE TYPE                            
004200     03 IDFAKT               PIC S9(7)           COMP-3.                  
004300*                                 FAKTURANUMMER                           
004400*                                 INVOICE NO.                             
004500     03 IDKUNDRF             PIC X(10).                                   
004600*                                 KUNDENS REFERENS (ORDERID)              
004700*                                 CUSTOMER REFERENCE (ORDER ID)           
004800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
004900*                                 KOLLINUMMER                             
005000*                                 CASE NUMBER                             
005100     03 IDARTNR              PIC S9(9)           COMP-3.                  
005200*                                 ARTIKELNUMMER                           
005300*                                 PART NUMBER                             
005400     03 KVANTAL              PIC S9(7)           COMP-3.                  
005500*                                 DATAELEMENT                             
005600*                                 DATA ELEMENT                            
005700     03 IDUSER               PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900*                                 USER SECURITY-IDENTITY                  
006000     03 SUARTSTD             PIC S9(9)V9(2)      COMP-3.                  
006100*                                 SUMMA STANDARDPRIS RADVÄRDE             
006200*                                 SUM LINEVALUE STANDARD PRICE            
006300*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
