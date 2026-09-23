//W116J029 JOB (640W1160100W116J029,W100),'RTN W116S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*COUNTRYX2=&COUNTRYX2                                                         
//*                                                                             
//********************************************************************          
//* TO REMOVE COUNTRY INFO FROM POSITION 24 TO 27                               
//********************************************************************          
//SORT     EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W116PD29),DISP=SHR                         
//SORTIN   DD  DSN=&W116..W116S2.W11683(0),DISP=SHR                             
//SORTOUT  DD  DSN=&&W11683,                                                    
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//********************************************************************          
//* MQ -PARTINFO FULL FILE TO VIPS                                              
//********************************************************************          
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=&&W11683                                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SPAREPARTINFO                                          
¤MQMPROP Vidb_Source=Full                                                       
¤MQMPROP LoadType=Full                                                          
¤MQMPROP Market=&COUNTRYX2                                                      
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J029                                         
