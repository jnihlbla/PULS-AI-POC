//W6D10SV9 JOB (640W0020200W6D10SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.W6D1V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(W6D1ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1HCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6D1ICLU),DISP=SHR                            
//*                                                                             
//W6D1    EXEC WG01SIU                                                          
//SIU.W6D1V  DD DSN=WG01.QASE.W6D1V,DISP=SHR                                    
//SIU.W6D1AK DD DSN=WG01.QASE.W6D1AK,DISP=SHR                                   
//SIU.W6D1BK DD DSN=WG01.QASE.W6D1BK,DISP=SHR                                   
//SIU.W6D1CK DD DSN=WG01.QASE.W6D1CK,DISP=SHR                                   
//SIU.W6D1DK DD DSN=WG01.QASE.W6D1DK,DISP=SHR                                   
//SIU.W6D1EK DD DSN=WG01.QASE.W6D1EK,DISP=SHR                                   
//SIU.W6D1FK DD DSN=WG01.QASE.W6D1FK,DISP=SHR                                   
//SIU.W6D1GK DD DSN=WG01.QASE.W6D1GK,DISP=SHR                                   
//SIU.W6D1HK DD DSN=WG01.QASE.W6D1HK,DISP=SHR                                   
//SIU.W6D1IK DD DSN=WG01.QASE.W6D1IK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:44:05                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=W6D1,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W6D10SV9                                         
