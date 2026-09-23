000100 01  EKHT-W51061.                                                         
000200*                                 FEL-LOGGDATA TILL ECO98                 
000300*                                                                         
000400     03 EKHT-IDPGM           PIC X(8).                                    
000500*                                 PROGRAM IDENTITET                       
000600*                                 PROGRAM INTENTITY                       
000700     03 EKHT-DAREGDAT        PIC 9(8).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000900*                                 REGISTRATION DATE (YYYYMMDD)            
001000     03 EKHT-TIKLOCK         PIC S9(9)           COMP-3.                  
001100*                                 KLOCKSLAG (TTMMSSTH)                    
001200*                                 TIME OF DAY (HHMMSSTH)                  
001300     03 EKHT-IDSEKVNR        PIC S9(3)           COMP-3.                  
001400*                                 GENERELLT SEKVENSNUMMER                 
001500*                                 GENERAL SEQUENCE NUMBER                 
001600     03 EKHT-KDEKHHT         PIC X(3).                                    
001700*                                 EKONOMISK HUVUDHÄNDELSE                 
001800*                                 ECONOMIC MAIN EVENT                     
001900     03 EKHT-KDEKSHT         PIC X(3).                                    
002000*                                 EKONOMISK SUBHÄNDELSE                   
002100*                                 ECONOMIC SUB EVENT                      
002200     03 EKHT-KDEKNIVA        PIC X(5).                                    
002300*                                 EKONOMISK HÄNDELSENIVÅ                  
002400*                                 ECONOMIC EVENT LEVEL                    
002500     03 EKHT-IDDC-SEND       PIC X(2).                                    
002600*                                 SÄNDANDE LAGER                          
002700*                                 SENDING WAREHOUSE                       
002800     03 EKHT-IDDC-REC        PIC X(2).                                    
002900*                                 MOTTAGANDE LAGER                        
003000*                                 RECEIVING WAREHOUSE                     
003100     03 EKHT-IDNYCKEL1       PIC X(10).                                   
003200     03 EKHT-IDNYCKEL2       PIC X(10).                                   
003300     03 EKHT-IDVERNR         PIC X(8).                                    
003400*                                 VERIFIKATIONSNUMMER                     
003500*                                 VERIFICATION NUMBER                     
003600     03 EKHT-DAVERDAT        PIC 9(8).                                    
003700*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
003800*                                 VERIFICATION DATE (YYYYMMDD)            
003900     03 EKHT-KDPRODSL        PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTSLAG                             
004100*                                 PRODUCT GROUP                           
004200     03 EKHT-KDPSLLOC        PIC 9(2).                                    
004300*                                 PRODUKTSLAG LOKALT                      
004400*                                 PRODUCT GROUP LOCAL                     
004500     03 EKHT-IDARTNR         PIC S9(9)           COMP-3.                  
004600*                                 ARTIKELNUMMER                           
004700*                                 PART NUMBER                             
004800     03 EKHT-FLLSBOK         PIC X.                                       
004900*                                 LAGERAVBOKNING                          
005000*                                 STOCKUPDATING                           
005100     03 EKHT-KDVALISO        PIC X(3).                                    
005200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005300*                                 CURRENCY CODE BY ISO-STANDARD.          
005400     03 EKHT-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
005500*                                 VALUTAKURS                              
005600*                                 CURRENCY EXCHANGE RATE                  
005700     03 EKHT-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
005800*                                 ARTIKELPRIS NETTO                       
005900*                                 NET PRICE EACH   (FOB NET)              
006000     03 EKHT-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
006100*                                 ARTIKELNS SJÄLVKOSTNAD                  
006200*                                 COST OF SALES                           
006300     03 EKHT-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
006400*                                 ARTIKELSTANDARDPRIS                     
006500*                                 STANDARD PRICE                          
006600     03 EKHT-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
006700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006800*                                 AVERAGE COST FOREIGN CURRENCY           
006900     03 EKHT-PRINK           PIC S9(7)V9(2)      COMP-3.                  
007000*                                 INKÖPSPRIS                              
007100*                                 PURCHASE PRICE                          
007200     03 EKHT-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
007300*                                 DIREKT LÖN                              
007400*                                 SURCHARGE COSTS                         
007500     03 EKHT-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
007600*                                 DIREKT MATERIAL                         
007700*                                 SURCHARGE PACKING MATERIAL              
007800     03 EKHT-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
007900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
008000*                                 REMAINING OVERHEAD SURCHARGE            
008100     03 EKHT-KVANTAL         PIC S9(7)           COMP-3.                  
008200*                                 DATAELEMENT                             
008300*                                 DATA ELEMENT                            
008400     03 EKHT-SUBEL           PIC S9(9)V9(2).                              
008500*                                 SUMMABELOPP                             
008600*                                 SUM AMOUNT                              
008700     03 EKHT-IDCPYTXT.                                                    
008800*                                 COPYTEXT IDENTITET                      
008900*                                 IDENTITY OF A COPYTEXT                  
009000        05 EKHT-CT-IDSYSTEM  PIC X(4).                                    
009100*                                 VOLVO VCAS SYSTEMNUMMER                 
009200*                                 VOLVO VCAS SYSTEM NUMBER                
009300        05 EKHT-CT-IDPTYP    PIC X(3).                                    
009400*                                 POSTTYP                                 
009500*                                 RECORD TYPE                             
009600        05 EKHT-CT-IDVTYP    PIC X.                                       
009700*                                 POSTTYPSVERSION                         
009800*                                 RECORD TYPE VERSION                     
009900     03 EKHT-WDR901-DATA     PIC X(100).                                  
010000     03 EKHT-IDTRANS         PIC X(4).                                    
010100*                                 BILDNUMMER                              
010200*                                 SCREEN NUMBER                           
010300     03 EKHT-IDUSER          PIC X(8).                                    
010400*                                 ANVÄNDARENS SÄKERHETS ID                
010500*                                 USER SECURITY-IDENTITY                  
010600     03 EKHT-DAREGDAT-FEL    PIC 9(8).                                    
010700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
010800*                                 REGISTRATION DATE (YYYYMMDD)            
010900     03 EKHT-BEFEL           PIC X(50).                                   
011000*                                 FELTEXT                                 
011100*                                 ERROR TEXT                              
011200*** END OF VILMAII-COPY LENGTH= 324 BYTES                                 
