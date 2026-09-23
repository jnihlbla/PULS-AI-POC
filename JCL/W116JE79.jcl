//W116JE79 JOB (640W1160100W116JE79,W100),'RTN W116D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//* TO REPLACE ¤MQMPROP HEADERS BY ¤DAP HEADERS                                 
//SORT01   EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W116PF79),DISP=SHR                         
//SORTIN   DD  DSN=W116.W116D2.W1167B(+0),DISP=SHR                              
//SORTOUT  DD  DSN=&&W11679,                                                    
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN                                                    
//* MQ STEP                                                                     
//WQSEN  EXEC WZ11P023,                                                         
//            DSIN=W116.W116D2.W1167B(+0)                                       
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
/*                                                                              
//*   VCOM-D&P                                                                  
//VCOM    EXEC WZ14DAP4,DSIN=&&W11679                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116JE79                                         
