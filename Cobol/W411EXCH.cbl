000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411EXCH.                                                
000500 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000600 DATE-WRITTEN.   JANUARI   -02.                                           
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - PROGRAMMET RÄKNAR OM VALUTA FÖR DDI-MARKNADER.                   
001400*        RÄKNAR OM SEK TILL LOKAL VALUTA ELLER                            
001500*        LOKAL VALUTA TILL SEK BEROENDE PÅ KDCALL.                        
001600*        KDCALL = 1 - LOKAL VALUTA TILL SEK                               
001700*        KDCALL = 2 - SEK TILL LOKAL VALUTA                               
001800*                                                                         
001900*        LÄNKAREA: W411EXCH                                               
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W411EXCH'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003500*      --- VALID IDDC CODES                                               
003600*                                                                         
003610*01    -COPY WWDC99                                                       
003620       EJECT                                                              
003700     EJECT                                                                
003800 LINKAGE SECTION.                                                         
003900*                                                                         
004000*   -COPY W411EXCH                                                        
004100*                                                                         
004200     EJECT                                                                
004400 PROCEDURE DIVISION  USING EXCH-W411EXCH.                                 
004500                                                                          
004600 STYR SECTION.                                                            
004700                                                                          
004800     PERFORM A-INIT                                                       
004900                                                                          
005000     IF EXCH-KDCALL = +1                                                  
005100                                                                          
005110***    RÄKNA OM FRÅN LOKAL VALUTA TILL SEK                                
005111       IF EXCH-SUORDV-IN > +0                                             
005112         COMPUTE EXCH-SUORDV-UT ROUNDED = EXCH-SUORDV-IN *                
005120                                          EXCH-PRKURS                     
005130       END-IF                                                             
005140       IF EXCH-PRARTNTO-IN > +0                                           
005150         COMPUTE EXCH-PRARTNTO-UT ROUNDED = EXCH-PRARTNTO-IN *            
005160                                          EXCH-PRKURS                     
005170       END-IF                                                             
005200     ELSE                                                                 
005210       IF EXCH-KDCALL = +2                                                
005220                                                                          
005230***      RÄKNA OM FRÅN SEK TILL LOKAL VALUTA                              
005231         IF EXCH-SUORDV-IN > +0                                           
005232           COMPUTE EXCH-SUORDV-UT ROUNDED = EXCH-SUORDV-IN /              
005233               EXCH-PRKURS                                                
005234         END-IF                                                           
005235         IF EXCH-PRARTNTO-IN > +0                                         
005237           COMPUTE EXCH-PRARTNTO-UT ROUNDED = EXCH-PRARTNTO-IN /          
005238               EXCH-PRKURS                                                
005239         END-IF                                                           
005240                                                                          
005250       END-IF                                                             
005260                                                                          
005270     END-IF                                                               
006200                                                                          
006300     GOBACK                                                               
006400     .                                                                    
006500     EJECT                                                                
006600                                                                          
006700 A-INIT          SECTION.                                                 
006800                                                                          
007000     MOVE +0                   TO EXCH-SUORDV-UT                          
007100                                  EXCH-PRARTNTO-UT                        
007600     .                                                                    
007700     EJECT                                                                
007800                                                                          
